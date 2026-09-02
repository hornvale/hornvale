# The Penstock: The Derived Working Set — A Program Metaplan

**Status:** Draft for G3 review (2026-08-22) · **Author:** Claude
(campaign-autopilot) · **Decider:** Nathan

> A **program metaplan**, not a campaign spec — the shape of
> `2026-07-11-the-walk-metaplan-design.md` and
> `2026-07-14-ecs-program-metaplan-design.md`. It fixes the commitments a
> sequence of campaigns share and sets the standing gate. Each campaign then
> gets its own spec → plan → execute cycle off this document.

## 1. What this is

The ledger is the only stored truth, and everything else re-derives. That is
settled (The Walk §3.2/§3.4, UNI-20, decision 0037). What is *not* settled is
what happens to a derivation after it is computed: today, almost always,
nothing. It is recomputed on the next call.

This program gives derivations a **place to live and a lifetime**: a working
set of materialized views and derived components over the append-only fact
ledger, lazily populated, versioned by ledger position, bounded by a
deterministic budget, and dropped when cold. An entity, in this frame, is not
a row anywhere — it is whichever derived components currently carry its key,
condensed out of the log when someone looks and dissolved when nobody does.

**Nothing here is greenfield, and nothing here is a new source of truth.**
The Walk §3.6 already sanctioned the destination by name: *"An ECS-shaped,
archetype-indexed materialization of current state is welcome as a **derived
cache** for hot iteration … but it is a read-optimisation, reconstructible
from the log — never the source of truth."* The lazy-and-incremental half
already ships at index granularity (`kernel/src/ledger.rs:140`: the
permutation indexes are `#[serde(skip)]`, "rebuilt on first use after load,
maintained incrementally on commit. Absent-or-complete"). What is missing is
per-query views, dense iteration, and a lifecycle.

## 2. The spine: two mechanisms that must never be one

This is the organizing axis of the whole program, and conflating the two
halves is the single failure mode that would cost determinism.

**(a) The view cache is invisible.** It changes speed and nothing else.
Evicting any part of it at any moment must be a provable no-op: same facts,
same artifacts, same agent decisions, byte for byte. Because it is invisible,
its eviction policy is unconstrained by world semantics and may use any
deterministic budget we like.

**(b) The working set is visible.** Which entities are *simulated at all* —
as opposed to sampled from a field — is observable, so that boundary must be
a pure function of world state: observer position, `WorldTime`, seed. Never
of memory availability, query history, or wall time. It is diegetic, and it
is exactly The Walk §4.1's refinement protocol ("focalized, deterministic,
contradiction-free detail at (location, agent, time), stored only if it is an
irreversible divergence") and the ECS metaplan §8's "lazy and
observer-relative — game as lens."

A single policy cannot be simultaneously required to be invisible and
required to be visible. Two mechanisms, two policies, one shared store.

**The consequence for scale is the reframe this program is really built on.**
A million agents should not exist as entities. `MAP-7` already ships the
alternative — population as a closed-form field, sampled at O(1) anywhere,
with settlements as condensations. So the target is not "a million entities,
fast." It is: *the condensed set is dozens to low thousands of real entities,
derived on demand and cached; everything else is a field sample, never
instantiated and never ticked.* "Can we run a million interacting agents"
decomposes into two better questions — how large the condensed set can be at
a given tick rate (a perf question, this program) and how faithfully the
field stands in for the rest (a Lab study, not this program).

## 3. Core commitments

Fixed for every campaign in the program.

1. **The ledger is the only stored truth.** Nothing in this program is
   serialized. No save-format change, no epoch suffix, no cross-version
   hazard — a new derived component is a pure read over existing facts. This
   is the program's largest flexibility dividend and it is structural.
2. **VIEW ≡ SCAN.** Every materialized view returns exactly what the naive
   scan returns, in the same order. This generalizes the existing INDEX ≡
   SCAN battery (`kernel/src/ledger.rs`, `index_equals_scan_*`) and it is the
   keystone: the whole performance story rests on this one property.
3. **Deterministic inputs only.** The policy may read live-view count,
   queries since last use, facts appended since materialization, and
   `size_of` estimates. It may not read `Instant`, `SystemTime`, or real RSS
   — banned by `clippy.toml` under decision 0001. Wall-clock may *inform* a
   human-set constant, measured in the `heavy:` tier, exactly as the ECS
   metaplan §6 already splits deterministic budgets from wall-time micros.
4. **Position-versioned, never time-versioned.** A cache entry records the
   ledger position it was derived at plus its dependency key. Append-only
   (`Ledger`: "facts are never mutated or removed") makes prefix-folds
   monotone, so an entry is valid until a later fact touches its dependency
   set. Non-monotone derivations — anything latest-wins, e.g.
   `latest_value_of` — declare themselves explicitly.
5. **Scope to the unit of work.** TOOL-24's standing discipline: *"reject a
   global/LRU cross-world provider cache — the census builds each world once,
   so it gains nothing, risks OOM across thousands of worlds, and stands up a
   second source of truth against seed+ledger; reuse scopes to the unit of
   work."* A session for `possess`; within-world for the census. This
   program does not weaken that rule; it gives it a mechanism.
6. **Closed view set before open.** The author declares which view *shapes*
   exist. A workload-authored (database-cracking) tier is deferred, not
   dropped — a closed set keeps VIEW ≡ SCAN enumerable and therefore
   testable, which an open set does not.

## 4. Measured baseline

**Provenance, because the numbers drive the stage order.** A throwaway
`kernel/examples/` probe, `--release`, on the Mac (not the canonical box),
synthetic ledger of `agent-at` facts, deleted after the run. These are
order-of-magnitude readings for sequencing decisions, not committed
baselines. `docs/timings.md` remains the ledger for anything durable.

```
  agents     hist      facts      scan_ms     index_ms      view_ms
     100      100      10000        13.32         0.24         0.14
    1000      100     100000      3913.91         5.14         1.72
    1000     1000    1000000     48287.88        44.40        17.81

  scan  = find(pred).filter(subject == e), per agent   <- what liveness.rs does
  index = facts_about(e).filter(pred),     per agent   <- the existing SPO index
  view  = materialized per-subject view, rebuilt fresh each time

  size_of::<Fact>() = 104 bytes, plus two heap Strings (predicate, provenance)
  seed 42's whole world: 12,534 facts / 995 entities (~12.6 facts per entity)
  -- counted from the committed cli/tests/fixtures/world-seed-42.json (3.1 MB)
```

Three findings, in descending order of importance.

**The largest available win is a call-site fix, not a subsystem.**
`windows/vessel/src/liveness.rs:958` and `:1230` run
`ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity)` — a scan of every
agent's entire history, per agent, per tick, which is quadratic in session
length. The existing SPO-backed `facts_about(e).filter(|f| f.predicate == …)`
is **1,088× faster at 1M facts** (48,288 ms → 44 ms) and adds no machinery.

**Corrected count: FOURTEEN call sites existed, of which two are fixed and
twelve remain.** (An earlier correction said "twelve exist, two fixed, ten
remain" — itself wrong, and wrong in an instructive way: the enumerated list
below is of the sites STILL UNFIXED, so the two already repointed are
additional to it, not among it. An off-by-two that survived a review, a fix
brief and a fix wave before a re-derivation caught it.) The plan's own single-line, single-predicate grep found five
matches and named two as production; completeness is what enumeration gets
wrong, and a broader scan (every predicate, spanning `.filter(|f| ...)`
across lines) finds twelve production sites (before the `#[cfg(test)]`
module): `:127` `latest_committed_position`, `:849` `agent_sightings`,
`:936` the thirst read in `drive_at`, `:1042` `build_emitter_scan`, `:2257`
`fatigue_at`, `:2442` the hunger read, `:3710` `affect_of`, `:4154`
`room_entry_day`, `:4430` `last_fact_day_at_or_before`, and `:4909`/`:4916`/
`:4923` inside `WalkState::begin` (DRANK/RESTED/EATEN) — twelve sites, all
still unfixed. The two this campaign fixed (`:958`, `:1230`, above) are
additional to that list, and are the least hot of the fourteen:
several remaining sites run **once per creature per tick** — the three in
`WalkState::begin` and `room_entry_day` — and `latest_committed_position`
runs once per creature **per band member** it is compared against, which is
worse than either. See §6.2's corrected suspect for the sites this drives
directly (`shared_believed_water`, `build_emitter_scan`,
`alarm_field_memo`).

**Against the corrected baseline, a materialized view buys 2.5×** (44 ms →
17.8 ms), and that figure already includes rebuilding the view from scratch.
The *lifecycle* — budget, hysteresis, eviction — competes only for the space
between 17.8 ms and 44 ms. It is the least-justified part of the idea, not
the most, and it is staged last accordingly.

**Views win big only on the axes with no index.** The three permutation
indexes key on subject, predicate and object; `place` and `day` are not index
keys at all, so "who is in this room" is a full scan today:

```
    1000     1000    1000000       139.17            —          8.51   16.4x
```

16× at only 64 queries, and the gap widens linearly with query count (scan is
O(facts × queries); view is O(facts + queries)). With a thousand agents each
asking "who is near me," the same shape projects to roughly 250×. Locality
queries are what *interacting* agents are made of.

## 5. The architecture

### 5.1 The rate table, extended

The ECS metaplan's rate table is the substrate's organizing lens. This
program adds a fourth rate — and the novelty is that its lifetime is measured
in **queries**, not in world time or build time.

| tier | changes… | storage | in the save? |
|------|----------|---------|--------------|
| build-static | never (authored) | typed registry | no (re-derived) |
| sim-dynamic | over sim time | Fact ledger | **yes** (only truth) |
| derived | per query (ephemeral) | recomputed | no |
| **working-set** | **per query-epoch** | **the view store** | **no** |

### 5.2 What gets a view

Only shapes the permutation indexes do not already serve, ranked by the
measured gap: `place`-keyed (locality), `day`-keyed (as-of and history
windows), and multi-predicate archetypes ("entities carrying components X and
Y"). A shape the SPO/PSO/OSP indexes already answer in O(log n + k) does not
get a view; it gets a correct call site.

### 5.3 Eviction

Count-budgeted, not byte-budgeted: cap the number of live views (the bound is
on what the company holds in repertoire, not on the size of the theatre). A
count is deterministic by construction, so no pressure signal is needed at
all rather than merely being forbidden. Eviction is **two-tier** — drop the
dense payload first and keep the key set, so re-warming is an array fill
rather than a fresh index descent. Admit and evict thresholds must differ:
a view that becomes cheap invites more querying of its own shape, so the
cache changes the workload it measures, and equal thresholds flap.

### 5.4 The condensation boundary

The observable half of §2. A named, pure, testable function of (observer
position, `WorldTime`, seed) deciding which entities are condensed out of the
field into real derived entities, and which dissolve back. Death is a fact;
the tombstone is the *cache's* record that an entity's components are void —
a cached absence, distinct from "not looked up yet," which is precisely the
distinction `RoomMeshMemo` already draws.

### 5.5 Placement, and a contract problem to solve

Kernel gets the domain-agnostic mechanism (the view store, position
versioning, the budget); domains declare their view shapes; worldgen and the
windows own the working set. One known obstacle: `TickSystem::step(&self,
frozen: &Ledger)` has nowhere to hang a cache, so `DriveMovements` builds a
throwaway `RoomMeshMemo` and `HomeNavCache` on every call and documents that
it cannot do otherwise without a kernel-trait change. The tick contract needs
a place for a view store or systems will keep paying per call.

### 5.6 Plans are not views, and the two halves land on opposite sides

A* and GOAP results are the obvious next tenants of this store, and half of
them belong here. The split is not by cost; it is §2's visible/invisible axis
again.

**A view projects the ledger. A plan projects the ledger *plus a goal*.** That
is a different key space — `(agent, goal, start, world-slice)` rather than a
query shape — and, more importantly, a different invalidation semantics. A
view over an append-only log is monotone: valid until a later fact touches its
dependency set. A path has **two** failure modes, and only one of them is
detectable that way. It can become *impassable* (a fact touched the route —
catchable by watching the dependency set), or it can become *suboptimal* (a
shortcut opened somewhere the route never goes). Nothing in the path's own
dependency set witnesses the second; you would have to re-search to find out.

**And that is where the answer inverts, because suboptimality is not a
defect.** An agent walking the long way because it does not know about the new
bridge is not a stale cache — it is a *belief*, and this project has an
epistemic layer for exactly that (`UNI-16`, `UNI-1`, The Surmise). So a plan
must NOT be silently refreshed the way a view is: a stale plan is observable,
and it is content.

Consequences, and they are concrete:

- **Spatial paths sit on the visible side, with the working set.** They may
  not be evicted under a memory budget, and their staleness policy must be a
  pure function of world state. The unification is The Quickening's rule
  applied one level up: the discrete divergence — *this agent resolved to go
  there, on this day* — commits, and the step sequence stays derived and
  re-derivable from that commitment. So a path uses the **same
  position-versioning machinery as a view, anchored to the intention's ledger
  position rather than to "now."** Same mechanism, different anchor.
  Replanning becomes an in-world event with an in-world trigger (the agent
  perceived an obstruction — a fact), never a cache miss.
- **GOAP plans sit on the invisible side and fit the view machinery
  cleanly.** A GOAP plan's dependency set is a set of *predicates*, and
  `CapabilitySchema` (`kernel/src/schedule.rs`) already declares which system
  writes which predicate. The invalidation key a view would need is therefore
  already authored, for free.
- **D\* Lite / LPA\* are probably the wrong import**, and this is worth
  writing down before someone reaches for them. Their entire value is
  maintaining optimality as the world changes cheaply — which is precisely
  what an agent who should not know about the change must not do. They are
  right for the player's own pathing, and for an agent that genuinely
  perceives the change; they are wrong as a general policy, and adopting them
  wholesale would quietly delete the belief layer.

Existing machinery to build on rather than replace: `HomeNavCache` already
keys on `(pos, home, budget, avoid-epoch)` and already counts its searches as
a deterministic witness; `astar.rs`'s `Solver`/`SearchSpace` seam already
threads an optional `RoomMeshMemo` and its doc already contemplates widening
that memo type "when a second domain wants a different memo." This store is
that second domain.

### 5.7 Replan triggers: the invalidation key IS the monitor

A creature that reconsiders every tick is both expensive and stupid — it
cannot hold a long mission. The alternative is **rationale-based execution
monitoring**: watch only the conditions the plan's rationale depended on, and
otherwise commit. That monitor set is precisely §5.6's dependency key, so one
mechanism serves both — but only after a distinction the obvious three
examples quietly straddle.

**Invalidation and preemption are different, and only one of them is this
store's job.**

| trigger | what actually changed | mechanism |
|---|---|---|
| a door opens on the route | the plan's **preconditions** | **invalidation** — dependency set touched; replan the *same* goal |
| a bugbear appears | nothing about the plan; a **different goal now outranks it** | **preemption** — goal arbitration, then plan afresh |
| health drops below 50% | likewise: the plan still executes fine | **preemption**, on internal state |

Two of the three are not invalidation at all. The plan remains perfectly
valid; what changed is which goal deserves the agent. Conflating them costs
both directions: replanning the same goal when the agent should switch goals,
or treating every salience change as a cache miss and thrashing. Invalidation
belongs to the plan store; preemption belongs to the motivation engine
(`PSY-6`), and the two meet only at "the agent needs a plan now."

**A threshold is a discrete event, not a polled predicate.** "Health below
50%" must not be re-evaluated every tick for every agent. Commit the
*crossing* as a fact and let the trigger watch that — the divergence-commits
rule (The Quickening) applied to a continuous quantity. The threshold is where
the smooth becomes discrete, which is exactly what the ledger is for.

**Dispatch invalidation from the commit, never poll it from the agent.** The
naive monitor replaces N replans with N × |deps| checks per tick, which is
worse. Invert it: when a fact commits, look up which plans registered a
dependency on that (subject, predicate, place) and mark only those dirty. The
existing permutation indexes are already that dispatch table. Cost becomes
O(dependents of the touched fact) rather than O(agents), and it is the same
position-versioned machinery §5.6 needs anyway.

**Triggers evaluate against perception, not the ledger.** A bugbear that has
not been *seen* must not preempt anything, or every creature is omniscient.
`observe` already ranks by salience through a `PerceptionLens` with a
`VISIBILITY_FLOOR` (`kernel/src/phenomena.rs`), so the preemption ranking and
the perception gate are the same existing call. This is the same trap as
§5.6's optimality one, one level over: the mechanism must run over what the
agent can know, never over what is true.

**"Instant" means the next tick, and that is a real constraint.** The tick is
bulk-synchronous — every system reads the frozen tick-N snapshot and writes
land in N+1 — so a fact committed during tick N is not visible to a monitor
until N+1. One tick of latency is the default; same-tick reaction is the
documented opt-in that needs the topological schedule (ECS metaplan §3.6,
§4.6). A design that assumes zero-latency reaction is assuming the opt-in.

Three counters fall out, and they belong in stage 1's instrument set:
invalidations dispatched per tick, replans per agent per hundred ticks, and
the **preemption-to-invalidation ratio** — which says whether creatures are
being interrupted by the world or by their own appetites.

### 5.8 Scope note: preemption is `PSY-6`'s, and it mostly ships

§5.7 over-reached, and the correction matters for the stage list. Preemption
is **not** this program's to build. `arbitrate` (`windows/vessel/src/
liveness.rs:3277`) already resolves competing drives — utility as Σ active
capped-urgency × serviceability, survival capped above comfort ("soft Maslow,
no priority table"), committed with hysteresis, and gated on `&Perceived`
rather than the ledger, so §5.7's perception rule is already enforced by the
type. The bugbear is already a drive: `Danger` carries a threat niche,
`boldness` (banked `threat_response`), alarm contagion from nearby frightened
creatures, and remembered dread — a believed phobia distinguished from sensed
alarm by provenance alone. A health threshold is another drive, and the
two-threshold dead-band that stops it thrashing shipped with The Wanting.

**The Penstock's whole interface to preemption is therefore two things:** the
invalidation half (§5.6, §5.7), and the counters — invalidations per tick,
replans per agent per hundred ticks, and the preemption-to-invalidation ratio.

**One gap is real, and it is the one a long mission needs.** All of the above
arbitrates what to do *next tick*; nothing protects a *multi-tick plan*.
`time_horizon` looked like the commitment dial and is not — it is
`anticipation_lead`, which lowers the act threshold so a far-sighted creature
acts on a need it can project. It makes creatures act **earlier**, never
persist **longer**. The only commitment input `arbitrate` takes is `incoming:
Mode`, hysteresis on switching, which knows nothing about how far through a
journey the creature is. Absent a progress-weighted term, a creature
interrupted often enough completes nothing — **preemption starvation**, the
precise failure "let creatures hold long missions" is meant to avoid.

### 5.9 The capstone study: is the sunk-cost bias adaptive?

The commitment ratchet — commitment rising with sunk progress — is introduced
above as an engineering fix. It is also, exactly, **the sunk-cost fallacy and
the escalation of commitment**, which turns a tuning constant into a
scientific object and gives this program a terminal payoff worth naming now
even though it is stages away.

**Preregistered hypothesis** (0016; frozen here, before the code that moves
it): in an environment with noisy threat signals and non-zero switching costs,
the commitment slope maximizing total need-satisfaction is strictly **greater
than zero** — a sunk-cost bias is *adaptive*, not merely irrational — and the
optimum rises with both signal noise and switching cost.

- **Independent variable:** commitment-ratchet slope, 0 (memoryless) upward.
- **Dependent:** mission-completion rate, threat-response latency, deaths,
  and total need-satisfaction as the fitness measure.
- **Environment parameters:** threat-signal noise; switching cost.
- **Falsified if** the optimum sits at slope 0, or the response is monotone
  with no interior optimum. A null here is the headline, not a failure.

**What makes it more than tuning: the knee of that curve is the boundary
between a virtue and a pathology.** Left of it, persistence is adaptive
commitment; right of it, the same mechanism is escalation of commitment.
Locating that point is a result, not a parameter choice.

Two caveats worth freezing with the hypothesis. **Roster power**
(`MEM-roster-power`): the sweep needs creatures actually occupying both ends
of `threat_response` and `deliberation_latency`, or one side of the
comparison has no sample and the study is unmeasurable for reasons that look
like a null. And **Buridan's ass cannot occur here** — a deterministic total
order with seed tie-breaks means perfect indifference never paralyses an
agent; it resolves arbitrarily but reproducibly. That is a design position
worth stating rather than a happy accident, and it means the study measures
persistence, never paralysis.

Precedent that this kind of result is reachable: **learned helplessness
already ships** as an emergent sticky scar in `arbitrate` — a creature whose
survival drive has gone unmet long enough stops trying, behaviourally
distinct from a merely frustrated one. The capstone would be the second
instance of that pattern, not the first.

## 6. The campaign carve

Strangler-fig; each stage shippable, reversible, and measurement-gated on the
one before it.

| # | campaign | delivers | gate to enter | verdict (2026-08-23) |
|---|----------|----------|---------------|-----------------------|
| **1** | **The call sites and the instruments** | the `liveness.rs` fix; a public indexed (subject, predicate) multi-fact query; facts-per-agent-per-tick as a deterministic gated counter; an agent-scaling bench (N agents × T ticks → ticks/s, bytes/agent, and the query/plan/commit split); a synthetic-world generator that reaches large entity counts without paying genesis | — | — |
| 2 | Views on the unindexed axes | `place`- and `day`-keyed views; VIEW ≡ SCAN battery; chaos-eviction harness | stage 1 shows locality queries are a real share of tick cost | **NOT ENTERABLE** — §6.5 measured ledger queries at 0.04% of a tick, not a real share |
| 3 | The derived-component layer | fan-out materialization, position-versioned, tombstones; reuse-before-eviction instrumented | stage 2's measured reuse ratio justifies the memory | — |
| 4 | The lifecycle | count budget, two-tier eviction, hysteresis | stage 3 shows the rebuild pass actually dominates | — |
| 5 | The condensation boundary | the observable half: condense/dissolve as a pure function; the field/entity fidelity study | stages 2–4, plus a Lab study design | — |
| 6 | Row width | predicate interning, provenance interning-or-drop | measured — plausibly a larger memory win than all of 2–4 | — |
| 7 | **Log bounding** | a fact-lifetime mechanism: what may leave the log, and how the seed plus the surviving prefix still re-derives the world | stage 1's facts-per-agent-per-tick counter shows a long session accumulates without bound | **DEFERRED ON AVAILABILITY, NOT MERIT** — the gate is met (§6.1) and §6.3 calls this "the one item in this program with no alternative," but it lives entirely in `windows/vessel/src/liveness.rs`, held off twice over: `campaign/the-hand` holds off `windows/vessel/` and `windows/lab/`, and The Escapement's Task 9 is mid-sweep of 53 `.day()` sites in that same file. Recommended next the moment vessel frees. |
| 8 | Storage tier (deferred) | decision 0037's address-prefix partition, disk paging, segment merging | 0037's own condition: a long-running deployment measures a ledger that will not fit | — |

**2026-09-02, stage 7 progress.** 7a's primitive shipped as The Tailrace (2026-08-26) and its tenants migrated as **The Pawl** (`docs/superpowers/specs/2026-09-01-the-pawl-design.md`; chronicle `book/src/chronicle/the-pawl.md`): the `liveness.rs` folds now read a session-owned resident store, byte-identically, and the thirst and hunger reads fell 230x. 7b (the typed intention) is the recommended next campaign and 7c stays behind it per decision 0238. The stage-7 row above is left as it was written on 2026-08-23; this is an annotation, not a rewrite.

**Stage 7 is not part of stage 8, and separating them is a correction this
plan needed.** "LSM machinery" reads as one deferred bundle, but decomposing
it gives eight parts — append-only log, sorted derived indexes, persistence,
segmentation, compaction, tombstones and GC, read-path skipping, leveling —
of which Hornvale already has the first three. Of the rest, **compaction's
semantic half is independently motivated by the in-memory case and cannot
wait for disk.** The ledger is append-only and never removes facts, so a
session running for hours with many agents grows without bound *in RAM*; the
working set does not help, because the thing growing is the log itself. Every
sibling architecture (event sourcing, Datomic, WALs, git's packfiles) answers
this with a snapshot or checkpoint so readers need not replay from zero.
Hornvale has the strongest possible checkpoint for build-state — the seed
re-derives everything — and **none at all for a long session's accumulated
sim history.** `MEM-1`'s melt (fact → phenomenon → myth) is already the
diegetic form of exactly this mechanism, which is a strong hint that the
non-diegetic form should be designed alongside it rather than invented twice.

Stage 1 is deliberately not a cache. It is a bug fix and a set of
instruments, and it exists to tell us whether stages 2–4 are worth anything.
If the instruments say locality queries are noise next to GOAP planning, this
program should stop at stage 1 and say so.

### 6.1 MEASURED, 2026-08-22: the stage-7 falsifier fired

Stage 1's counter is in (`windows/vessel/tests/suite/tick_commit_budget.rs`,
commit `1f0a6465b`). Seed 42, default `PossessOpts`, 7 agents, 40 ticks.

**Lead with the code trace — it is the load-bearing evidence; the
measurement below is corroboration, not the argument itself.**
`agent_at_fact` is pushed unconditionally on every `MoveTo`
(`liveness.rs:5094`); there is no divergence test, so one fact commits per
step of every walk. The Quickening's rule — *only the discrete divergence
commits; the smooth routine stays derived* — was implemented as "the latest
committed `agent-at`, **else the derived schedule**," and that worked because
a fixed two-point schedule was the default a divergence could be measured
against. The Wanting, Foresight and Temperament replaced that schedule with
drives. **No campaign broke the rule; the ground it stood on was removed**,
and with no default to diverge from, everything commits. Unboundedness
follows directly from that trace plus one constitutional fact: the ledger is
append-only (it never removes a fact), so a non-summable per-tick commit
rate — one that never trends toward zero — means the log grows without
bound for as long as agents keep walking. That conclusion does not depend on
any particular measured number; it depends only on there being no divergence
test in the commit path, which the trace above establishes directly.

The measurement corroborates rather than carries the argument:

```
  first-half rate  0.950000 facts/agent/tick
  last-half rate   0.928571 facts/agent/tick
  100-tick run     both halves flat at 0.948571
```

**It holds flat. It does not fall toward zero.** §11's second falsifier is
therefore live, in its own words: *"If a realistic session outgrows RAM
before query cost ever matters, stage 7 becomes the program and everything
above it is premature."* At ~0.94 facts/agent/tick and ~200 bytes of real
`Fact`, a thousand agents over ten thousand ticks is ~9.4M facts — roughly
1.9 GB — and it never stops growing, because the log is append-only.

**Consequence: §5.6 is the fix, not merely a caching note.** It already says
a path anchors to the ledger position of a committed *intention* and
re-derives its step sequence, because the planner is deterministic. Read as a
log-bounding mechanism rather than a cache mechanism, that is exactly the
missing default: commit *"resolved to go to the spring on day D"* once, and
re-derive the N steps between intentions instead of storing them. The saving
is the mean path length. Stage 7 and §5.6 are the same idea reached from two
directions — which neither this spec nor its author saw when they were
written, and which the measurement forced.

**This does not re-order the stages by itself.** Stage 1's remaining
measurement (the query/plan/commit split) can still say the whole read side
is noise. But stage 7 is no longer speculative, and any future reading of
this program should start here rather than at §6's table.

### 6.2 MEASURED, 2026-08-22: the query/plan/commit split, and the verdict on §11

Stage 1's last instrument (`windows/vessel/examples/agent_scaling.rs`,
commits `43d851a5e`/`60237bbb5`). Seed 42, 221 settlements, real agents via
`derive_npcs` with `k` as the sweep variable, 20 ticks, `--release`:

```
  agents    ms/tick   facts/a/tick  search/a/tick    total_bytes
      10     68.216       2.2150        1.7750           1720654
      50    503.724       2.8250        2.2790           2106071
     100   1267.741       2.6780        2.0675           2512286
     200   5722.748       2.8960        2.2822           3519536

  fitted slope, ms/tick vs agents        1.43
  per-segment   10->50  50->100  100->200
    run 1        1.24     1.33     2.17
    run 2        1.27     1.11     1.99
  marginal bytes/agent  ~9,468
```

**§11's first falsifier does NOT fire: plan cost does not swamp query cost.
The program continues past stage 1.** State the reasoning honestly, because
it is indirect: both *directly measured* terms — plan (`HomeNavCache::
searches`) and commit (ledger delta) — are **flat per agent**, i.e. linear in
agent count. Total tick cost is **superlinear**. Neither measured term
explains the excess, so the unmeasured residual is the plausible driver.
**That is an argument from elimination, not a measurement of query cost**,
and no stage should treat it as the latter. It also carries an unstated
premise: a flat *count* per agent (searches, commits) implies a flat *cost*
per agent only if per-unit cost is itself constant — and for commit it
demonstrably is not, since `Ledger::commit`'s idempotency and contradiction
checks grow with ledger size, so a flat commit *count* need not mean a flat
commit *cost*. The verdict survives that gap anyway, because §6.2's
corrected suspect (below) supplies a mechanism for the superlinear residual
that is independent of both the count-vs-cost premise and the query-cost
question this argument from elimination could not directly answer.

**The single fitted exponent understates the trend.** The curve accelerates:
the 100→200 segment is 2.17 and 1.99 on two runs — near-quadratic across the
range that actually matters — while the fitted 1.43 averages that away. Any
plan built on "1.43" is planning for a gentler world than the measured one.

**A named suspect, corrected.** An earlier draft of this section claimed
`hazard_memory_memo` (`liveness.rs:1219`) and `alarm_field_memo` (`:3907`)
are whole-population reads performed *per creature*. The code contradicts
that: `alarm_field_memo` is called once per tick, before the per-creature
loop (`liveness.rs:4670`, the call site's own comment says so in capitals),
and `hazard_memory_memo`'s whole-population half (`build_emitter_scan`,
called at `:1249-1252`) is memoized per `t` via `PrimaryAfraidMemo`, so it
too runs once per tick — neither is quadratic *in invocation count*.

Both functions **are** quadratic in agent count, but for a different reason:
**unindexed per-member subject scans**, not per-creature invocation.

- `liveness.rs:1042` (`build_emitter_scan`): `ledger.find(AGENT_AT).filter(|f|
  f.subject == m.entity)` inside `for m in roster` — a full ledger scan per
  roster member, once per tick.
- `liveness.rs:3916` (`alarm_field_memo`): `agent_position(frozen, npc, day)`
  per npc, which reaches `latest_committed_position` (`:127`) — the same
  unindexed scan, once per npc per tick.
- **The largest, and previously unnamed: `shared_believed_water`
  (`liveness.rs:1366`)**, called once per creature per tick from
  `WalkState::begin` (`:4931`), which loops the *whole band* calling
  `agent_position` per member (`:1380`) — **O(A²) per tick**. An earlier
  draft wrote O(A²·k) "where k is band size", which overstates it by a power
  of A: `WalkState::begin` is called as
  `WalkState::begin(frozen, npc, &self.npcs, …)` from inside
  `for npc in &self.npcs` (`:4713`), so the band IS the whole roster and
  k = A. The ranking is unchanged — this is still the worst of the three —
  but the magnitude is not what that notation claimed.

So the superlinear residual is most likely **the same defect Task 2 fixed,
at sites Task 2 missed** — more `find(pred).filter(subject == e)` call sites
needing the same `facts_of`/indexed-query swap, no new machinery — rather
than a locality-query shape that needs a `place`-keyed view. This
**strengthens** the §11 falsifier-1 verdict (the program continues): it
supplies a concrete, cheap mechanism for the residual instead of leaving it
an unexplained shape. Stage 2 must confirm this — not the withdrawn
per-creature-invocation theory — before building anything, because a wrong
suspect would send it at the wrong axis (a `place`/`day` view instead of the
remaining unindexed-scan call sites).

**A discrepancy that stays open, deliberately.** This bench reports 2.2–2.9
facts/agent/tick where §6.1's instrument reports 0.93–0.95 — the same named
metric, ~3× apart. Ruled out with code evidence: tick semantics (both build
`DriveMovements` over one `WorldTime` day), the `TURNED_HOSTILE` pass
(bounded, and it can only *raise* the lower figure), `Session::wait`'s double
evaluation (only the second commits), `absorb_here`, and tick count.

The **leading** hypothesis is tighter and arithmetic, not compositional:
**a denominator artifact.** `tick_commit_budget` divides by all 7 agents
(3 peopled + 4 wild); this bench's `k` sweep divides by `k` real agents only
and excludes wild fauna from the denominator entirely. 7/3 = **2.333**, and
the measured ratio 2.2150/0.9486 = **2.335** — the two agree to three
figures. The mechanism is concrete: `windows/vessel/src/clock.rs:183`
`cost_ticks` scales action cost by `tempo(mass_kg)`, so heavier wild beasts
act less often and commit fewer facts per tick while still occupying
`tick_commit_budget`'s denominator — deflating its rate by roughly the
peopled-agent share of the roster. This is testable in minutes (partition
fact deltas by subject and recompute the rate over peopled agents only) and
should be checked before stage 2 leans on either instrument's magnitude.

**Roster composition remains a secondary, unquantified hypothesis**:
`ordered_for_derivation` sorts settlements population-descending with only
home pinned, so §6.1's instrument samples the three largest plus wild fauna
while this one reaches into small marginal settlements with none — and
species, mass and distance-to-water all feed the action clock. It may
compound with the denominator artifact rather than substitute for it.

Either way, **the direction is reassuring**: both hypotheses point at
§6.1's rate being *understated* relative to a peopled-agent-only accounting,
which means the memory projection §6.1 draws from that rate is conservative,
not optimistic — strengthening, not undercutting, §6.1's conclusion.
§6.1's conclusion is unaffected in shape either way: it rests on the *shape*
(flat, not falling), which both instruments agree on; only the magnitude
differs.

**What §11's falsifiers say now.** §11 lists four. Two are answered by the
measurements above: falsifier 1 (query cost is a minority of tick cost) does
**not** fire, per §6.2 — the program continues past stage 1. The
log-bounding falsifier (the unbounded log binds first) **did** fire, per
§6.1 — not by stopping the program, but by promoting stage 7 to something
this program should start from rather than defer to. The remaining two are
still open, untouched by this measurement round: reuse-before-eviction is
**structurally untestable at stage 1** — nothing here builds a working set
to measure reuse against — and the row-width win (stage 6) is simply
unmeasured.

One further arithmetic note, verifiable from figures already in this
document: §4 gives `size_of::<Fact>() = 104` bytes as the fixed struct cost,
and §6.2's marginal figures (`~9,468` bytes/agent over `2.896` facts/agent/
tick × 20 ticks ≈ `57.9` facts/agent) give a marginal cost of
`9467.8 / 57.9 ≈ 163` bytes/fact. The difference, `163 - 104 ≈ 59` bytes/
fact, is the heap portion (`predicate` + `provenance` + any `Value::Text`
object) — roughly a third of the marginal per-fact cost. This document does
not have a further breakdown of those 59 bytes across the three heap
sources, so it stops short of translating that into a stage-6 win estimate;
that split is stage 6's own measurement to make.

### 6.3 MEASURED, 2026-08-23 (The Scour): the suspect is confirmed, and mostly gone

§6.2 named twelve unindexed `find(pred).filter(|f| f.subject == e)` sites as
the likely driver of the superlinear residual, and required stage 2 to
**confirm or kill that suspect before building anything**. The Scour
repointed all twelve onto `Ledger::facts_of` — twelve one-line swaps against
a method that already existed — and re-ran the bench.

```
                      BEFORE (2 runs)      AFTER (2 runs)
  fitted slope         1.43 / 1.52          1.12 / 1.11
  tail, 100->200       2.17 / 1.99          1.41 / 1.29
  ms/tick @ 200 agents    5722.7            1752.2 / 1712.6
```

**3.3× at 200 agents, and the near-quadratic tail is largely gone.** The new
values sit outside the old runs' spread, and the absolute change is far
beyond run-to-run noise.

**Behaviour-preserving, by a stronger check than the test suite.** Every
deterministic counter the bench reports is **byte-identical** across the
change — facts/agent/tick (2.2150 / 2.8250 / 2.6780 / 2.8960), searches
(1.7750 / 2.2790 / 2.0675 / 2.2822), `total_bytes`, and the raw fact and
search counts. Only wall time moved. `lens_purity` and the 21 deliberately-
naive scans left in the `#[cfg(test)]` module as an independent oracle agree.

**Consequence for the program, and it is a subtraction.** The case for
stages 3–5 — the cached working set, `place`-keyed views, the eviction
lifecycle — rested on a superlinear read cost that is now mostly removed by
call sites rather than machinery. **Do not build them on the strength of
§6.2's numbers; they are superseded.** A residue remains (1.11–1.12 fitted,
1.29–1.41 in the tail), so something superlinear is still there, but it is a
much smaller quarry and it has not been attributed. Stage 3 should open by
measuring *that*, not by building against it.

**Unchanged, and worth stating because it is easy to read this section as
better news than it is.** Facts committed per agent per tick is *identical*
before and after — this campaign touched reads, not writes. §6.1's finding
stands untouched: the ledger still grows without bound, and stage 7 remains
the one item in this program with no alternative.

### 6.4 THE LEVEL IS ALARMING, AND §6.2 ANSWERED THE WRONG QUESTION ABOUT IT

Raised by Nathan on reading §6.3, and it is correct: **1752 ms/tick at 200
agents is 8.76 ms per agent per tick**, for one creature deciding one step,
in a world with 221 settlements and a handful of drives. At the small rung it
is 57.5/10 = **5.75 ms per agent-tick**. Most of the cost is therefore *not*
scaling — it is a constant, and it is already large when the content is thin.

**§6.2's verdict conflated slope with level, and must not be relied on as
written.** It concluded "plan cost does not swamp query cost" from this
reasoning: plan and commit are flat *per agent* while total cost was
superlinear, so neither explains the *excess*. That is an argument about
**shape**. §11's falsifier asks about **level** — *does planning dominate
tick cost?* — and no measurement on this branch answers it. With the
superlinearity now largely removed (§6.3), what remains is precisely a large
constant, which is exactly where planning would sit unobserved.

What is actually known per agent-tick at the 200 rung: **~2.28 A\* searches**
and **~2.9 fact commits**, inside 8.76 ms. Nothing on this branch times
either. If a room-graph search costs 1–2 ms, planning alone is most of the
budget and §11's falsifier-1 *does* fire — the opposite of what §6.2 records.

**The cheap experiment that settles it, and stage 3 should run it first.**
The counters exist; the timings do not. Time the A\* path and the commit path
inside the bench harness (which already carries the wall-clock exemption), and
the residual stops being "everything else" and becomes attributed. Until then,
treat §6.2's falsifier-1 verdict as **UNSETTLED**, not as the go-ahead it is
phrased as.

**RESOLVED by §6.5, added directly below this section in the same commit
(`06a07efcc`) — a reader following this paragraph today would re-run a
measurement already taken.** The profile times the A\* path exactly as
prescribed: **5.3%** of the `agent_scaling` bench. That settles §11's
falsifier-1 in the direction §6.2 originally recorded, but on firmer
grounds — planning does **not** dominate tick cost. The large per-agent-tick
constant this section is alarmed about is real, but it lives in derived
geometry (`NearestCellIndex::scan_at`, 13.4%) and the allocator
(`malloc`+`memcpy`, 33.3%), not in GOAP search. Nothing here should send a
later campaign back to "time the A\* path" — that experiment has been run.

**Why this matters beyond bookkeeping.** A per-agent-tick cost of ~6–9 ms
puts roughly 100–170 agents in a 1 s tick budget, before the content gets
richer — and richer content is the whole direction of travel. Whatever the
split turns out to be, the level is the binding constraint on agent count long
before the slope is, and no cached-view layer addresses a constant that lives
in planning.

### 6.5 PROFILED, 2026-08-23: where the time is, and why 0.04% must NOT retire the read side

Two profiles (`samply`, `--profile profiling`, this Mac), one per workload,
because the two regimes differ so much that neither generalises:

```
  possess, 17-command script          agent_scaling bench, 200 agents
  ------------------------------      -------------------------------
  67.9%  build_world (genesis)        13.4%  NearestCellIndex::scan_at
   2.6%  TICK                          5.3%  A* (AStarSolver::solve)
  26.3%  libm transcendentals          ~5%   terrain::branch reads
   9.7%  malloc                       33.3%  malloc + memcpy
                                       0.04% ledger queries
```

A session start is **genesis-bound**; a long tick loop is bound by derived
*geometry* and by the **allocator**. 93% of `scan_at` arrives via
`RoomAddr::corner_weights` from the drive stack — `TOOL-24`'s open lever #1,
independently reached.

**The 13.4% survived a harness-artifact challenge — a finding that lived only
in commit `06a07efcc`'s message until now.** The bench originally built its
terrain with `LocaleTerrain::new`, which hard-codes `cache: None` — a
configuration `Session` never actually runs. That shape is visible in the
harness, and it should raise the same doubt in the next reader that it raised
here: is `scan_at`'s cost a real cost, or an artifact of benching a
misconfigured terrain? Wiring in the cache production actually passes moved
`scan_at` only from 14.9% to 13.4%, so the harness-artifact hypothesis was
wrong and the cost is real. The remaining divergence (`Session` also
prefills the memo once per tick, which the bench does not) is a documented
lower bound on the gap rather than a guess.

**THE 0.04% IS TRUE AND MUST NOT BE USED TO RETIRE STAGES 3–5.** An earlier
draft of this section did exactly that, and Nathan refused it correctly. The
figure measures a simulation in which **agents barely read about one
another**: each reads ~2.3 facts about *itself* per tick and essentially
nothing about anyone else. The mature workload this program exists to serve —
belief as a fold over perceived events (`UNI-16`), social edges as a fold over
events between a pair (`SOC-9`), GOAP planning over belief rather than truth,
a creature reacting to who just walked in — makes every agent read about
**M others**. Read cost goes from N to **N×M** with M rising from ~0. That is
a new term, not a larger constant, and no measurement taken today can bound
it.

**The falsifiable form, so a later campaign can settle it rather than
re-arguing it:** ledger-read share should scale with (agents × others each
agent reasons about × reconsiderations per tick). Re-measure when belief or
the social graph first ships. If it is still ~0 with M genuinely non-zero,
*then* the read side can be retired on evidence.

### 6.6 KEEP IT GENERAL: two classes of derived value, one store

Nathan's standing direction (2026-08-23): *"however we benefit, we benefit —
keep the system general so we're sure to be able to use it for whatever comes
up."* The concrete content of "general" here is that the store must not be
specialised to whichever derivation happens to be hot this month.

| | **world-derived** | **ledger-derived** |
|---|---|---|
| examples | `domains/terrain`'s `rills_of`/`rill_reading`, terrain branch geometry | belief, social edges, positions, plans |
| a pure function of | (seed, place) | the ledger prefix |
| invalidation | **never**, within a world | when a later fact touches the dependency set |
| eviction | memory pressure only | pressure, or dependency touched |
| hot **today** | yes, ~5% of a tick | no, 0.04% |
| hot in the **mature sim** | unchanged | the N×M term above |

**Corrected 2026-08-23 (The Forebay, decision 0206): `corner_weights` was the
wrong example for the left column, and its absence here is not an oversight.**
It is not seed-scoped at all — `Geosphere::new` takes only a level, and two
geospheres at the same level are byte-identical, so `corner_weights` is a
pure function of `(RoomAddr, level)` with no world identity in its key
whatsoever. It is `Pure` in decision 0206's terms, but it is not an instance
of *this table's* "world-derived" category, which this section defined as
keyed by `(seed, place)`. The genuine seed-keyed tenant that belongs here is
`domains/terrain`'s `rills_of`/`rill_reading`, which resolve from a `Seed` via
`CatchmentCut::Drawn` — roughly 5% of the `agent_scaling` profile (§6.5),
and unmigrated onto the `Derived` store The Forebay built. Decision 0206
formalises the split this table gestures at: two classes, `Pure` (a pure
function of its key, never invalidated) and `Ledger` (a fold over a ledger
prefix), drawn at **key-completeness** rather than at provenance —
"world-derived" is `Pure` with the world's identity folded into the key, not
a distinct mechanism from geometry that needs no world identity at all.

The storage is identical; **only the invalidation policy differs**. A store
built for the left column alone is a geometry memo and will need replacing. A
store whose entries each carry their own dependency key — empty for
world-derived, a (subject, predicate, place) set for ledger-derived — serves
both, and serves the ones nobody has thought of yet. Build that, and let the
first tenants be whichever is hot.

**A consequence worth stating: the allocator, not the recomputation, may be
the larger prize.** 33% of the bench is `malloc`+`memcpy`. Caching a value
avoids recomputing it; handing back a slice of a dense array avoids
*allocating* it. §1's "iterated as an array" clause was written as a
performance nicety and the profile suggests it is the main event.

### 6.7 SHIPPED, 2026-08-23 (The Forebay): the residual 13.4% is miss-bound, established by the profile

The campaign this section's own direction motivated (§6.6) shipped
`kernel/src/derived.rs`'s `Derived<K, V>` and migrated `RoomMeshMemo` onto it
(decisions 0206–0208). Its own headline question — does the migration retire
a real share of `scan_at`'s 13.4%, or is the store built for a workload that
has not arrived yet — needed a hit-rate reading nothing in the tree
provided. The wire route to `campaign/the-hand` (the owner of
`agent_scaling.rs`) expired unanswered, and a board `ask` went unanswered
through the task's end, so the number came from a kernel-side synthetic
probe (`kernel/examples/room_mesh_memo_hitrate.rs`) rather than from the real
bench: **96.2%** hit rate under a locality-shaped synthetic walk.

**That number is weaker evidence than it looks, and the campaign said so of
itself.** With nothing evicting, the hit rate is arithmetically
`1 - distinct/total` — a restatement of the probe's own walk width, not an
independent confirmation. The probe is a negative control (it rules out "the
memo is broken and never hits"), not a measurement of `agent_scaling.rs`.

**The number that actually settles it was already in hand, in this
section's own profile, and nobody had drawn the inference.**
`agent_scaling.rs` hoists `mesh_memo` above its tick loop (`:375`), so §6.5's
13.4% was measured against an **already-warm** cache — every tick after the
first reuses whatever the walk revisited. A warm cache that still costs
13.4% is miss-bound by direct observation on the real workload, with no
synthetic distribution involved. §6.5 stated the figure without drawing this
inference; The Forebay draws it now, retroactively, as this section's own
correction.

**Consequence for a later campaign.** The store ships regardless (§6.6's
direction is generality, not a claim on today's 13.4%), and the honest
follow-up for the residual is a **faster `scan_at`**, or a **reachable-set
prefill** — not a cache lifecycle, since the memo already has one and misses
persist under it. The number that would size a prefill is
**distinct-rooms-per-tick**, not a hit rate; a future campaign with access to
`agent_scaling.rs` should read that directly rather than re-deriving it from
a synthetic probe, once `windows/vessel` frees. A second, cheaper-to-confirm
finding from the same instrument: `agent_scaling.rs` clones the whole memo
every tick (`let mesh_snapshot = mesh_memo.clone();`) to satisfy a borrow,
and `malloc`+`memcpy` at 33.3% of the bench makes that full-`BTreeMap` clone
a candidate contributor in its own right — a property of the harness, not of
the sim, and worth measuring before concluding either way.

## 7. The standing gate

**Determinism.** Byte-identity of committed artifacts before and after, every
stage (the drift check already runs it). A behaviour-preserving change is
byte-identical, so every stage passes it unchanged. The stage-1 call-site fix
must additionally assert **order** equivalence, not merely count equivalence:
`positions_for_predicate` and `positions_for_subject` both return ascending
positions (commit order), and `facts_about_yields_commit_order_not_index_key_order`
already pins that semantics — but a fix that silently reordered results would
change downstream iteration and is exactly the kind of thing a count-only
assertion misses.

**Correctness — the ladder, strongest first.**
1. *Type-level*: the view store hands out immutable borrows; a `&self` query
   cannot mutate the ledger it reads.
2. *Property*: **VIEW ≡ SCAN** over random ledgers, plus rebuild ≡
   incremental (the property `fact_index.rs` already claims for itself).
3. *Adversarial*: **chaos-eviction** — evict at every legal opportunity and
   assert byte-identical output. This is what strict invisibility (§2a) buys,
   and it is why invisibility is a capability rather than only a restriction.
4. *Master oracle*: the census drift check.

**Perf.** Deterministic budgets (view counts, fact counts, `size_of`) are
drift-checked and gate-able. Wall-time micros use the existing exempted
harness pattern — `cli/tests/suite/session_cost.rs` and
`windows/vessel/examples/turn_cost.rs`, with their scoped
`#[allow(clippy::disallowed_types)]` and the standing justification ("times
derivation calls for a diagnostic — never sim logic, never a fact, never
seeded from wall-clock").

## 8. Determinism contracts

- **No serialized surface.** Nothing this program builds enters the save. If
  a stage ever needs to serialize a view, that is a new decision, not an
  implementation detail.
- **Eviction is a no-op, provably.** Any observable difference between an
  evicted and a resident run is a bug of the highest severity, not a
  tolerance.
- **The working-set boundary is a pure function of world state.** Memory
  availability may never reach it.

## 9. In / out

**In:** the derived working set — views, derived components, their lifetime,
the condensation boundary, and the instruments that gate all of it.
**Out:** the fact ledger's on-disk representation (stage 8, and 0037's gate);
the field/entity fidelity question (a Lab study); anything client-side
(0022/0023).

## 10. Decisions this program will need

Numbers assigned at ratification, not here — `docs/decisions/` currently tops
out at 0159 and parallel campaigns are minting.

1. **The derived working set is two mechanisms, not one** (§2). The
   invisible/visible split, and the rule that a policy may not be both.
2. **Cache entries are versioned by ledger position, never by time** (§3.4).
3. Whether decision 0037 needs amending to record that its in-memory tier now
   has a named working-set layer above it — probably a *see-also*, not an
   amendment, since 0037's own scope is storage.

## 11. What would falsify this program

Stated up front, per decision 0016, because a metaplan that cannot be wrong
is not a plan:

- **Stage 1's split shows query cost is a minority of tick cost.** If GOAP /
  A* planning dominates, stages 2–4 are optimizing noise and the program
  should stop.
- **Measured reuse-before-eviction is low.** The fan-out is a cost multiplier
  before it is a saving; a working set whose components are read once or
  twice before dissolving is strictly worse than recomputing.
- **The row-width win (stage 6) exceeds everything stages 2–4 buy.** Entirely
  possible, and it would re-order the program.
- **The unbounded log turns out to bind first.** If stage 1's counter shows a
  realistic session outgrows RAM before query cost ever matters, stage 7
  becomes the program and everything above it is premature.

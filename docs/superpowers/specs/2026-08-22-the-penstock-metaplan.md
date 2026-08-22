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
`windows/vessel/src/liveness.rs:956` and `:1228` run
`ledger.find(AGENT_AT).filter(|f| f.subject == npc.entity)` — a scan of every
agent's entire history, per agent, per tick, which is quadratic in session
length. The existing SPO-backed `facts_about(e).filter(|f| f.predicate == …)`
is **1,088× faster at 1M facts** (48,288 ms → 44 ms) and adds no machinery.

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

| # | campaign | delivers | gate to enter |
|---|----------|----------|---------------|
| **1** | **The call sites and the instruments** | the `liveness.rs` fix; a public indexed (subject, predicate) multi-fact query; facts-per-agent-per-tick as a deterministic gated counter; an agent-scaling bench (N agents × T ticks → ticks/s, bytes/agent, and the query/plan/commit split); a synthetic-world generator that reaches large entity counts without paying genesis | — |
| 2 | Views on the unindexed axes | `place`- and `day`-keyed views; VIEW ≡ SCAN battery; chaos-eviction harness | stage 1 shows locality queries are a real share of tick cost |
| 3 | The derived-component layer | fan-out materialization, position-versioned, tombstones; reuse-before-eviction instrumented | stage 2's measured reuse ratio justifies the memory |
| 4 | The lifecycle | count budget, two-tier eviction, hysteresis | stage 3 shows the rebuild pass actually dominates |
| 5 | The condensation boundary | the observable half: condense/dissolve as a pure function; the field/entity fidelity study | stages 2–4, plus a Lab study design |
| 6 | Row width | predicate interning, provenance interning-or-drop | measured — plausibly a larger memory win than all of 2–4 |
| 7 | **Log bounding** | a fact-lifetime mechanism: what may leave the log, and how the seed plus the surviving prefix still re-derives the world | stage 1's facts-per-agent-per-tick counter shows a long session accumulates without bound |
| 8 | Storage tier (deferred) | decision 0037's address-prefix partition, disk paging, segment merging | 0037's own condition: a long-running deployment measures a ledger that will not fit |

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

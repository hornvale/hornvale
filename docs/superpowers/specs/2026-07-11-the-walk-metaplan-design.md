# The Walk Metaplan: Someone Walks In It — Design

**Date:** 2026-07-11
**Status:** Approved (brainstorming session)
**Parent spec:** `2026-07-05-hornvale-longterm-plan-design.md` (Constitution §2 governs; §3.5 "The Game" and the Year-3 "someone walks in it" roadmap are the endpoint this drains toward)
**Worktree:** `the-walk` (branch `the-walk`), off `main` at `68dd1ce`
**Provenance:** The Year-3 metaplan (`2026-07-09-year-3-metaplan-design.md`) chose "the world has a past" as its spine and **explicitly deferred the epistemic second ledger** (naming it the presumptive Year-4 spine, MAP-2). This metaplan takes up that deferred arc ahead of schedule and supersedes its "second ledger" framing (see §3.2, §10).

---

## 1. What this is

A **metaplan**, not a single campaign: the arc from the world Hornvale has today
— a complete *static* cross-section, generated at genesis and queried through the
almanac, REPL, atlas, and orrery — to a world someone can **walk in**. It is
five-to-eight campaigns spanning two milestones, deliberately decomposed into
fine, CI-cheap, parallelizable chunks. Each campaign named here gets its own
spec → plan → execution cycle; this document fixes the spine, the load-bearing
decisions, the seam, and the structure that lets the campaigns run concurrently.

The governing observation: **almost none of this touches worldgen.** The game
layer is new crates and windows that *consume* the existing sim's read surface.
It sidesteps the census/drift load that makes worldgen changes expensive, so it
can be chunked finely, turned around quickly, and interleaved with the
worldgen-heavy worktrees (`crust`, `room-scale-variety`) without collision.

## 2. The spine: the game is loop-closure

Every client of the sim runs one loop:

```
  Query -> Focalize -> Act -> Commit -> Advance -> (Query ...)
```

Today the sim runs this loop **once, at genesis, then halts** — it only ever does
*Query → Render*. The almanac is that loop cut open and flattened. **The game is
what closes it.** Every missing subsystem is one arc of this circle, and the
event ledger sits at the hinge (Commit): both the player work and the time work
pass through it.

This reframing is stronger than a two-axis "Time vs Player" split, because the
two axes are just phases of the *same* loop — Advance is time, Query/Act is the
player — and it unifies six roadmap items that looked far apart (see §3.1).

## 3. Core commitments

Five decisions settle the shape. The first three are **new decisions to record**
in `docs/decisions/`; the fourth resolves a standing frontier question; the fifth
is a deferral.

### 3.1 The PC is a promoted, seeded NPC — no authored opening

You do not wake up in a cart bound for Helgen. The world is already populated
with situated agents; **starting the game is a control transfer onto one of
them, in medias res.** The camera descends onto one agent in the crowd — a
goblin, a drow, a dragon — and hands you the controls.

This is not a skin over the sim; it is the sim's own logic. The longterm plan
already says (§3.1.6, line 151): *"an observed dragon is a full agent. Live
behavior must refine, never contradict"* the coarse statistical self. The player
is the observed agent, promoted.

Consequence — the **situated-agent abstraction is a single spine**, and six
roadmap items are substitutions on it (who drives × vantage):

| who drives   | vantage             | = what it becomes                    |
|--------------|---------------------|--------------------------------------|
| the player   | embodied / local    | the game (the walker)                |
| the player   | remote / aggregate  | the scholar / ethnographer loop      |
| the sim      | embodied / local    | an NPC — this *is* "liveness"         |
| the sim      | remote / aggregate  | a faction / "tides of empire"        |
| a historian  | backward in time    | retrospective confabulation          |
| the culture  | across generations  | cultural memory (the memory economy) |

Player and NPC are therefore **one build**: liveness is a substitution away from
the player once the spine exists, not a distant separate axis.

**Generality test:** a goblin embedded in a caste ladder and a solitary,
long-lived dragon must be the *same* agent abstraction — only the vectors differ.
The dragon is the standing stress case.

### 3.2 One event ledger; belief is a derived view (not a second source of truth)

There is a single ledger of ground-truth happenings. **What an agent knows and
believes is *derived* from it** — never a rival store of truth. Three mechanisms,
arriving in tiers:

- **Projection** (perception): what an agent has observed is a lossy, spoofable
  projection of the events in reach. This alone gives *ignorance* — the fog —
  knowledge as a subset of truth, so coarse-constrains-fine holds for free.
- **Inference** (belief): what an agent *concludes* from its perceived events.
  This is where **false belief** lives — not a stored falsehood, but inference
  over truthfully-perceived (and possibly *deceptive*) events. A rumor is a *true
  event* (someone really said it); believing it is inference over the perceived
  utterance. This is the *inference program*, frontier UNI-1.
- **Belief-events** (the causeless cases — innate prior, dream, charm, authorial
  injection) are first-class events in the *same* ledger, so even these carry
  provenance.

Belief may be **materialized as a per-agent cache** for performance, but the cache
is always reconstructible from the ledger — derivation memoized, not a second
source of truth (per §3.4: belief is re-derivable, hence derived).

**Deception falls out of this, not out of a second ledger.** Every trick — lie,
illusion, disguise, forged evidence, charm — plants a *real* event or object in
ground truth; the victim perceives it truthfully (spoofable perception) and infers
a falsehood (credulous inference). Two payoffs a stored-belief model forgoes: (1)
**provenance** — `why does the guard think you're a priest?` → "he saw you in
priest robes on day 412"; (2) **automatic invalidation** — drop the disguise and
his belief corrects on next look with no bookkeeping, and one illusion-event fools
a whole crowd in O(1) where a stored model would write and later hunt down N
beliefs.

This **supersedes MAP-2's "provenance-tagged second ledger"** framing: one ledger,
belief derived — provenance is the *causal chain*, not a tag. **Tiering:** the
walkable frozen slice (Milestone 1) needs only pure projection (no one lies in a
frozen, single-agent world); the inference tier (UNI-1) and belief-events arrive
with the social/deception features in Milestone 2, so the architecture costs
nothing early.

### 3.3 Surprise is epistemic, not stochastic

Determinism stays constitutional. Novelty comes from the player not *yet* knowing
the deterministic truth — not from randomness. NPCs can be fully deterministic
agents and still feel alive; no RNG is introduced, none is permitted. The past a
player is dropped into is **derived** (confabulation + fields of history), not
simulated forward — "in medias res" costs no RNG and breaks no rule.

### 3.4 The seam boundary rule: store the irreversible, derive the reversible

The eager/lazy boundary (the keystone, §4) is drawn by reversibility: **store the
irreversible (player-caused, append-only events); derive the reversible
(everything re-derivable).** This resolves frontier **MAP-20** (the
runtime-vs-knowledge boundary), which asked for exactly this line to be *"drawn
once, deliberately, as a spec question when a campaign first pulls for
object-scale representation, not arrived at by drift."* The game layer is that
campaign.

### 3.5 Framing is built agnostic (conceit now, diegesis deferred)

The control-transfer mechanism is identical whether possession is diegetic (the
PC is a wandering godlet riding mortals — threading into the religion domain) or a
fourth-wall conceit ("you control an NPC"). **v1 builds the mechanism with no
in-world commitment and reads as a clean conceit.** Diegetic possession is
preserved as a later option (VI), foreclosed by nothing.

## 4. The keystone: the two-directional seam

Hornvale's worldgen is **bottom-up and eager** (Dwarf-Fortress-like: a global
scaffold computed at genesis). The game layer is **top-down and lazy**
(Caves-of-Qud/Minecraft-like: local detail elaborated on demand, only where
observed). These meet at a seam the constitution already governs — "coarse
constrains fine" *is* this seam — and both halves already exist in the kernel:

- `field.rs`: *"typed, lazily-evaluated functions over (space × time)… pure: same
  (pos, time) → same value, always,"* labelled *"the coarse in
  coarse-constrains-fine."* The top-down, location-deterministic prior. Cheap
  because it is a pure function, O(1) anywhere — not a simulated grid.
- `refine.rs`: *"pick deterministically among candidates that contradict
  nothing."* The constrained, on-demand elaborator (copy-on-write).

What is missing is the **protocol** that wires them for the game. Abstracted, the
seam is *copy-on-write, referentially-transparent, level-of-detail refinement* —
the shape of mip-mapping/LOD, progressive JPEG (every zoom is a complete valid
answer), materialized-view-vs-computed-on-read, sparse files, and lazy
evaluation. Caves of Qud is the reference hybrid.

### 4.1 The refinement protocol (the contract Chunk 0 encodes)

```
GOAL: focalized, deterministic, contradiction-free detail at (location, agent,
      time), stored only if it is an irreversible divergence.

  1. Resolve coarse context    sample fields + committed facts overlapping here
                                 [total: never fails -> tier-0 always works]
  2. Check the ledger ratchet   already elaborated/committed here? -> return it
                                 [determinism of observe -> leave -> return]
  3. Seed the elaborator        from world-seed (X) hash(location, entity, epoch)
                                 [order-independent = referential transparency]
  4. Elaborate candidates       fine detail as f(coarse context, local seed)
  5. Constrain (the boss fight) keep only what contradicts nothing coarse;
                                 all contradict -> widen / back off to coarser
                                 [refine.rs; never contradict, never crash]
  6. Project (the keystone)     filter through the agent's perception + reach
                                 [knowledge = projection; can only subtract]
  7. Write back divergences     append an event iff the player *changed* something
                                 [minimal ledger growth; MEM-1 memory economy]
```

Steps 3–6 for distinct locales run **independently → parallel by construction.**

### 4.2 Boundary conditions, and how each is handled

- **Determinism of lazy generation** — same location → same detail regardless of
  visit order. Handled by step 3 (order-independent seed) + step 2 (the ratchet).
  Enforced by the "observe → leave → return" seam test.
- **Edge agreement** (adjacent locales must agree at shared boundaries) — handled
  by **sparse coupling**: a locale depends only on the globally-continuous coarse
  fields at its coordinate plus its own seed, never on neighbours' committed
  detail. Continuity comes from the fields, which are continuous by construction.
- **Contradiction at refinement time** (the recurring "boss fight," longterm plan
  §8) — handled by step 5's back-off: widen candidates, or fall back to coarser
  detail; never contradict, never crash.

## 5. The scope ladder

The game descends a ladder we already own the top of:

```
  scope             on-death (vessel)     status
  ----------------  -------------------   ------
  WORLD (god's-eye) [the save file]       BUILT — the almanac / REPL
  CULTURE (a people) steer the next       side-branch (faction agents)
  LINEAGE (a blood)  inherit the heir     side-branch (kinship, generations)
  AGENT (one body)   re-drop / world runs *** THE WALK *** — v1 default
                                           (Reload/save-scum column REJECTED)
```

The god's-eye we already have is the top rung; the whole arc **descends the scope
axis to a single Agent.** v1's cell is fixed: **Agent scope, seeded vessel,
re-drop on death.** Lineage/Culture rows are optional branches; the Reload column
is rejected by §3.1.

## 6. Exit criteria

- **Milestone 1 — the walkable (frozen) slice.** `hornvale possess --seed N`
  drops you into one agent; you `look`, `go`, and `examine` your way through a
  small local space that re-describes as you move, focalized through your species'
  perception and your agent's projected knowledge. The world does not yet move.
  This is a shippable, demonstrable ending on its own.
- **Milestone 2 — the living world.** The world moves without you: NPCs (the same
  agent spine) take deterministic daily actions committed to the event ledger; you
  perceive change on re-query; on death you re-drop into another agent and the
  world you left persists.

## 7. Structure: walking skeleton + parallel refinement streams

Fine chunks and concurrency come from a **walking skeleton**: build the *seam*
first — a tier-0 stub of every interface, wired end-to-end — then refine each stub
in its own lane, in its own worktree, landing independently.

```
  <> Chunk 0 — THE GAME SEAM  [cheap]
     interfaces + trivial tier-0 of each: stub Agent, identity Projection,
     god's-eye Vantage-query, one-line Focalizer, single-room Local-space.
     `possess` returns *something* end-to-end on day one.
     (Mirrors Campaign 1: tier-0 of every domain first.)

  A - Knowledge  projection tiers -> subset test   (the fog: ignorance only) [cheap]
  B - Sight      vantage query -> focalizer -> registers/anti-repetition     [cheap]  <- room-scale-variety
  C - Space      local-space model -> go/move -> examine       [wg-read, then cheap] <- crust
        === MILESTONE 1: walkable frozen slice = Seam + A + B + C ===
  D - Time+mind  event ledger -> confabulation -> liveness/NPCs -> re-drop
                 -> inference/belief tier (UNI-1): rumor, lies, deception
                 [event ledger is kernel/determinism-sensitive; rest cheap]
        === MILESTONE 2: living world = + D ===
```

**Parallel-safe:** after the Seam, lanes **A / B / C** run concurrently — each
refines a different stub behind a frozen interface, and each is a pure consumer,
so the census/drift load never fires. **D** waits on A maturing plus its own event
ledger.

**The parallelism is only as good as the interfaces.** The Seam's real job is
getting the five interface signatures right the first time; a mis-cut Projection
or Vantage-query makes the lanes collide at integration. The Seam is small in
code, large in consequence, and deserves disproportionate design care.

**The `room-scale-variety` and `crust` dependencies are inbound *feeds*, not
blockers.** Lanes B and C develop against today's terrain/room surface and absorb
the richer versions when they land — coarse-constrains-fine guarantees the richer
feed *refines* the rendering, never breaks it.

## 8. The campaign carve

Milestone 1 — the walkable slice:

- **I. The Vessel** — the promoted-NPC primitive + the projection ledger (§3.2,
  the keystone architecture lands here). Derive one addressable agent from the
  existing population; bind its perception vector; project the static world
  through it. *Exit:* interrogate one agent's knowledge, proven a strict subset of
  ground truth. *Leans on:* species (perception vectors), settlement (population),
  kernel ledger/fields.
- **II. Focalized Sight** — vantage queries through the projection → authored-
  feeling room prose (the anti-repetition mandate). *Exit:* `look` describes where
  you stand, non-repetitive, from field + ledger. *Leans on:* `room-scale-variety`
  worktree, the scene window.
- **III. The Walk** — walkable local space (the lazy elaborator, §4) + the verb
  loop (look / go / examine); moving re-focalizes. *Exit:* `possess` → move through
  a small local space; it re-describes; world still frozen. *Leans on:* terrain
  region-graph (read), `crust` worktree.

Milestone 2 — the living world:

- **IV. The Event Ledger** — happenings over `WorldTime` (the Commit hinge);
  projection redefined over events; player acts mutate the world; the confabulation
  seam opens. *Exit:* an action commits an event that later queries reflect; "why
  is X?" recounts events. *Leans on:* kernel Fact ledger, historiography.
- **V. The World Moves** — time-stepping + liveness; NPCs are the same agent spine,
  deterministic (§3.3); re-drop / permadeath become real. *Exit:* NPCs act daily;
  you perceive change on re-query; death → re-drop. *Leans on:* I (spine reused), IV.

Later / off the critical path:

- **VI. Drama** — quests, the delimited event-game (MAP-15), focalized ethnography
  as the play artifact (TOOL-2), optional diegetic possession (§3.5).
- **Side-branches** — the ecology crate (DOM-3); Lineage / Culture scope
  (Crusader-Kings & Paradox stances); tides-of-empire / fields-of-history.

## 9. Chunk 0 / The Game Seam, in detail

The foundational, CI-cheap deliverable. It defines five interfaces and gives each
a trivial tier-0, wired end-to-end so `possess` works on day one and every later
chunk refines one stub without touching the others.

- **Agent** — an addressable individual derived (lazily) from the population. *Tier
  0:* pick the flagship settlement's argmax cell, mint one agent with an id and a
  position.
- **Projection** — `knowledge = project(ground_truth, vantage, perception)`. *Tier
  0:* the identity projection (agent knows the coarse world facts at its location).
  The subset property is the interface's contract.
- **Vantage-query** — "what is observable from here, now, to this agent," as the
  refinement protocol (§4.1). *Tier 0:* return the coarse fields + overlapping facts
  at the position (steps 1, 6 only; no elaboration yet).
- **Focalizer** — render a vantage-query result as prose. *Tier 0:* one templated
  line ("You stand in <biome> near <settlement>.").
- **Local-space** — the walkable graph at human scale. *Tier 0:* a single room with
  no exits.

*Exit:* `hornvale possess --seed 42` prints a one-line focalized description of
where the promoted agent stands, and `whoami`/`knows` dumps its projected
knowledge, proven a subset of ground truth. Byte-deterministic; a gallery artifact
(a first "possession transcript") committed and drift-checked.

The five interface signatures are the design deliverable of Chunk 0's own spec;
they are named here, not frozen here.

## 10. Frontier bookkeeping

- **MAP-2** (epistemic layer) — **refined**: "provenance-tagged *second* ledger" →
  one event ledger with belief *derived* by projection + inference (§3.2);
  provenance is the causal chain, not a tag. Land projection in I; the inference
  tier in Milestone 2.
- **UNI-1** (the inference program) — **scheduled**: it is the belief tier of §3.2
  (turning perceived events into possibly-false belief), landing with Milestone
  2's social/deception features. Update the row from frontier to spec'd-for-M2.
- **MAP-20** (runtime-vs-knowledge boundary) — **resolved / spec'd** by §3.4's
  reversibility rule. Flip `raw` → `spec'd`, pointing at this document.
- **MAP-15** (the delimited event-game), **TOOL-2** (player-facing ethnography) —
  **absorbed** into VI (Drama).
- **DOM-3** (ecology crate) — noted as a side-branch; unchanged status.
- **New decisions to record** in `docs/decisions/`: (a) the PC is a promoted seeded
  agent, no authored opening; (b) one event ledger — belief is a derived view
  (projection + inference), never a second source of truth; (c) surprise is
  epistemic, not stochastic. (The seam boundary rule is recorded via MAP-20's
  resolution.)

## 11. Cross-cutting

**Determinism and testing.** The seam tests (observe → leave → return) are the
spine of this arc's test strategy, and they are already named in the longterm plan
(§6). Every projection carries a subset property test (knowledge ⊆ truth). Surprise
is epistemic (§3.3), so no test ever asserts randomness; the past is derived, so
"in medias res" is a determinism property, not a fixture.

**CI economy.** Nearly every chunk is a pure consumer of the existing read surface
— no census, no drift regeneration. The two exceptions are read-only (III reads
terrain) or determinism-sensitive but census-free (IV extends the kernel ledger).
This is why the arc is chunkable and interleavable with the worldgen worktrees.

**Constitutional compliance.** New crates/windows only; domains untouched (the game
consumes them). No new dependencies. No `HashMap`/`HashSet`, no wall-clock. The
event ledger (IV) is the one save-format-sensitive change and gets its own
determinism review, including the Lorenz guard-rail (never seed a chaotic
integrator from quantized ledger floats).

**Abortable at milestone boundaries.** Milestone 1 alone is a shippable, demoable
thing (you can walk a frozen world) even if Milestone 2 never happens — satisfying
the constitution's "any campaign abortable; the tier-0 cascade always works."

## 12. Process

This is a metaplan: each campaign (and Chunk 0) gets its own spec → plan →
execution → book/decision close, per the standing process. Chunks are kept small
deliberately — the finer the better, for turnaround and for parallel streams.
Lanes A/B/C run as separate worktrees after the Seam merges. The keystone risk
concentrates in Chunk 0's interface design, which is where review effort should
concentrate first.

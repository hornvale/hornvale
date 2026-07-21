# The Tidings — belief-sharing, the band as error-correcting unit

**Status:** spec (brainstorm complete; awaiting G3 review)
**Date:** 2026-07-21
**Campaign:** The Tidings (first social-cognition campaign; realizes
`SOC-belief-sharing`)
**Precedes:** implementation plan → execution → merge

## Problem

The six Temperament drives make a creature *feel*, and The Surmise made it plan
over **belief, not truth** (`believed_water` — a lossy fold over what the
creature has itself perceived, `None` when it has never seen water). But every
creature is epistemically **alone**. A creature that has never stood at water is
ignorant of it forever unless it stumbles on it; a lost, thirsty creature reads
chronic distress with no path out, even standing beside a neighbour who knows
exactly where the spring is. One mind's knowledge cannot yet relieve another's
ignorance.

The frontier names the missing layer: the **group as the error-correcting
unit**. A healthy band smooths its members' failures by *circulating what it
knows* — the individual self-scoring health metric becomes a **band** property.
This is the first campaign in which one creature's mind changes another's, and
the root of a whole social-cognition subtree (strategic sharing, reputation,
deception) of which v1 takes only the root: **naive circulation of true
belief**.

## The core result (from ideonomy)

Three findings shape the design.

- **Belief-sharing is a distributed-systems *anti-entropy* problem, not an
  epidemic with dice.** The naive reading — rumour as an SIR epidemic — wants a
  probabilistic spread, which needs RNG and breaks determinism. The right frame
  is a **conflict-free replicated join**: model the merge as a monotonic,
  commutative, idempotent set-union and multi-agent determinism holds *by
  construction* — order-independent, no RNG, seed-tiebroken. The determinism
  constraint does not fight the design; it *selects* the design.
- **Consensus is not health, and sharing cannot create truth.** Naive
  circulation spreads truth and error identically, so a band that *agrees* is
  not healthy if it agrees on a falsehood — the band metric must measure
  **divergence-from-truth (distress against the real world), never internal
  agreement**. And sharing only *distributes* knowledge, never manufactures it:
  one creature's observation heals `N` ignorant band-members. **Sharing
  amplifies observation** — that is the leverage, and the framing for promoting
  the health metric from an individual to a band property.
- **The band does not yet exist in the live world.** `derive_npcs` mints one
  NPC per settlement, each homed at a distinct room (`liveness.rs:2519`), and
  nothing in the tick reads another NPC's state. Live NPCs are **never
  co-located with each other**; the band substrate is the same "first true
  multi-agent perception" The Belonging explicitly deferred
  (`the-belonging-design.md:124-127`). So v1 ships the **sharing law** and
  demonstrates it where bands *do* exist — the synthetic health harness — while
  the live world stays byte-identical and dormant until the reserved
  population-field substrate lands, at which point sharing activates with no
  further change. (G3 decision #4.)

## Design

### 1. The share law (union-fill over co-located belief)

A **band** is a set of co-located agents — v1: agents sharing the same room
(`position`); a wider share-radius is reserved. For a located-belief kind (v1:
believed water), each agent's belief after reconciliation is a pure re-derivation
over an **extended candidate set**:

```
shared_belief(agent, band) =
    argmin_{ r in known(agent, band) } planned_hops(agent.home, r)     # ties: ascending RoomAddr
where  known(agent, band) = { a.believed_water : a in band, a.believed_water = Some(_) }
```

i.e. the band pools the water rooms its members know of, and **each agent
re-ranks that pooled set by its own nearest-to-home metric** — the exact metric
`believed_water` already uses (`liveness.rs:624-627`). This is one function,
generic over the located belief; v1 instantiates it for water only (G3
decision #3).

Its two effects fall straight out of the set-union:

- an **ignorant** agent (`None`) adopts the band's nearest-to-*its*-home known
  water — **the motivating case**: the lost creature learns "the water is
  north," its thirst becomes serviceable, its affect lifts from lost/Frustrated
  toward Searching/Eager;
- a **knowing** agent may learn of a **nearer** water it had not itself seen and
  re-rank to it — a strict improvement.

**Why union-fill, not a sticky register (G2 decision #2a):** in v1 every belief
is *true* — a real water room the sharer actually perceived; there is no
staleness and no deception yet. A union of true beliefs is therefore always
safe: it can only add correct options. Correcting a **wrong** belief is a
different operation — it needs a provenance/recency order so a `Some → Some`
overwrite is well-defined without RNG — and is deferred to the staleness /
deception work, not forced into v1.

### 2. Where it lives — the harness demonstration

Because live NPCs never co-locate (§ core result 3), the share law is defined as
a pure function and **exercised and measured in the synthetic health harness**
(`windows/lab/src/synthetic.rs`), where co-located bands are constructible and
the health metric already runs. `Scenario::simulate` gains a **belief-circulation
step** each tick: after each agent's `Perceived` view is assembled and before
`arbitrate`, agents sharing a room reconcile `believed_water` via the share law.
The law rewrites the derived `believed_water` on the view — it stores nothing
new (belief remains a UNI-20 derived view).

The demonstration extends the existing `a_stricken_and_a_healthy_people`
scenario (`synthetic.rs:502`): a lost, ignorant creature co-located with a
knowledgeable neighbour. **Sharing off** (the null): the lost creature reads
chronic thirst-distress (no known water). **Sharing on:** it adopts the
neighbour's water, thirst becomes serviceable, and the band's aggregate distress
drops. "A healthy band smooths its members' failures" becomes a *measured
result*, not a slogan.

### 3. The band health metric

The metric already aggregates distress over a set of creatures — `HealthReport`
(`windows/lab/src/health.rs:138`: prevalence, chronicity, recovery, by_cause,
by_species). v1 adds no new field; it makes the set *interactive*. The campaign's
headline readout is the **paired comparison** — band-with-sharing vs the
sharing-off null — showing lower prevalence/chronicity. The metric stays grounded
on affect, which is grounded on serviceability against the **true** world, so a
band cannot fake health by agreeing on a falsehood (consensus ≠ health holds
mechanically).

### 4. The live world and possession (unchanged in v1)

Live derived NPCs are never co-located, so the circulation step is a **no-op**
over the live population: live behaviour, almanacs, and the possession galleries
are **byte-identical**. Possession-mode player↔NPC telling is a *different*
channel (The Echo's `knowledge.rs` sentence transfer) and stays out of scope —
v1 is NPC-band circulation, demonstrated in the harness.

## Architecture

The share law is a pure fold over (co-location set × each agent's
`believed_water`) — no stream draw, no stored state, no epoch, no new predicate.
It lives beside the belief fold in `windows/vessel` (a `circulate`/`share_beliefs`
function generic over a located belief), consumed by the lab harness's
`Scenario::simulate` loop between view-assembly and `arbitrate`. The health
metric is untouched; a new/extended synthetic scenario and a study exercise the
paired comparison.

## Determinism

- **Order-independent by construction.** The merge is a set-union re-ranked by a
  deterministic `argmin` (planned hops, ties by ascending `RoomAddr`).
  Permuting the band, or the order in which agents reconcile, yields identical
  beliefs — no RNG, no read of tick order.
- **No stream draw, no epoch, no new predicate; zero save-format cost.** Belief
  stays a derived view (UNI-20); sharing only widens the candidate set the fold
  ranks over. Nothing new is serialized.
- **Genesis byte-identical. Live/possession galleries byte-identical** (no
  co-located NPCs → the circulation step is a no-op live). Only the lab study
  artifacts (the extended synthetic scenario + its health rows) change —
  regenerated and accepted at close.
- The single-drive `decide` path and every existing drive are untouched
  (sharing rewrites only `believed_water` before arbitration, using the same
  values `believed_water` could already return from a richer perception history).

## Model card

The Tidings v1 is **naive union-fill circulation of true spatial belief over
co-located agents**, demonstrated and measured in the synthetic health harness,
promoting the population health metric to an interactive band property. Belief
is shared; it is never faked, corrected, withheld, or doubted.

**Deferred (captured in `.superpowers/sdd/followups.md`):**

- **Live bands** — belief-sharing over real co-located NPCs, blocked on the
  reserved **population-field / multi-agent-perception** substrate (the one The
  Belonging deferred). When it lands, the share law activates unchanged.
- **Correcting a false belief** — the `Some → Some` overwrite, needing a
  provenance/recency order; arrives with **staleness / deception**.
- **Strategic / selective sharing, information-as-value, leaking** ("being seen
  is the dual of seeing") → `SOC-information-economy`.
- **Credulity / gullibility gating** (whether to believe what you are told) →
  **The Discernment** (UNI-16).
- **Additional located beliefs** (food / home / danger-source) — the mechanism
  is generic; v1 wires water only (G3 decision #3).
- **A wider share-radius** (perceive a neighbour a room away, not only
  same-room).

## Test plan

- **Unit (the share law, `windows/vessel`):** an ignorant agent adopts the
  band's nearest-to-*its*-home known water; a knowing agent re-ranks to a nearer
  shared water; an all-ignorant band and a singleton band are unchanged;
  **order-independence** (every permutation of the band yields identical
  beliefs); the seed tie-break fires on equal planned-hop distance (ascending
  `RoomAddr`).
- **Harness (`lab::synthetic`):** the extended `a_stricken_and_a_healthy_people`
  band — with sharing, the lost creature's affect lifts (thirst becomes
  serviceable) and band prevalence/chronicity drop **below** the sharing-off
  null.
- **Determinism / artifacts:** genesis byte-identical; the possession galleries
  byte-identical (live circulation is a no-op — no co-located NPCs); the drift
  check passes on everything except the deliberately regenerated lab study.
- **Null-control (monotone-improvement):** in v1 sharing **never increases**
  band distress (every shared belief is true) — the "ratchet." A test asserts
  the paired health comparison is a reduction (or equal), never a regression.

## Deferred (captured)

Routed to `.superpowers/sdd/followups.md` and, at close, into the retrospective's
follow-up section and the relevant frontier rows (`SOC-information-economy`, the
reserved population-field PSY row, `The Discernment`). No new registry row is
needed — `SOC-belief-sharing` moves `raw → in-progress → shipped` and its row
records the harness-only-until-population-field scope.

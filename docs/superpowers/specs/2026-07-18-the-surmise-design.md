# The Surmise — the perception→belief tier (UNI-1, first instantiation)

**Campaign:** The Surmise (The Walk, Milestone 2 — the living world; the fourth
liveness slice)
**Registry:** UNI-1 (the inference engine — first instantiation) · UNI-16 (the
immune model — its L2 credulity threshold reserved) · UNI-20 (the derived-view
architecture — belief is the next derived view)
**Status:** G3 draft (awaiting Nathan)

---

## 1. Summary

An agent stops magically knowing where water is. Today `decide(view, home,
water, …)` receives the true resource room as a ground-truth argument — the NPC
plans a perfect beeline to a spring it has never seen. The Surmise replaces that
ground-truth input with **belief**: what the agent knows about the world is a
**lazily-populated cache over the one true ledger**, populated by *perception*
(you learn water is at a room by having stood there), and the planner plans over
**belief, not truth**.

Water is a **region** — many sources scattered across the low ground — so belief
is a genuine **nearest-known-of-several**: the closest water the agent has
actually found. With truth static this slice, a populated belief is never wrong,
so the divergence between belief and truth is **ignorance** — and multi-source
makes ignorance *graded*, not binary: an agent may know a *far* source while a
*nearer* one sits unperceived, and two agents with different perceived histories
believe *different* nearest sources and beeline to *different* water. The keystone
— **BELIEF ≡ FOLD-OVER-PERCEIVED** — is that behaviour is a pure function of
perceived history: same world, same thirst, different sightings → different plan,
different destination. It is the twin of The Wanting's DRIVE ≡ FOLD.

This is the body-swap the reserved `decide(view, …)` seam was built for (The
Wanting decision #9; The Foresight decision #10): belief fills the view. It is
census-free, genesis byte-identical, adds **no new committed predicate and no
save-format change**, and touches `windows/vessel/` only — domains untouched
(The Walk §11). It is the first instantiation of UNI-1 (inference over a hidden
seeded ruleset) at the agent scale, and it lights the fuse for the L2 immune
tier (deception).

## 2. Background: where this sits

**The UNI-1 tier has levels** (the frontier essays' own sequencing —
`book/src/frontier/frontier.md`, "The immune model of belief" and "The
projection layer"):

- **L0 — Perception:** a lossy, spoofable filter over the true ledger. *(the
  substrate — "ships first")*
- **L1 — Belief:** a fold over *perceived* events; a cache over truth. **← this
  campaign (with L0)**
- **L2 — The credulity threshold:** signal-detection; deception, lies, illusion,
  charm (the immune model, UNI-16). *(next: "The Discernment")*
- **L3 — Projection:** the ruleset's output includes the inferrer's own
  psychology — monsters. *(later)*
- **L4 — Population belief:** contagion, R0, herd immunity. *(faction scale,
  later)*

The immune essay is explicit that belief lands *"atop the projection/perception
tier that ships first,"* and its own cheapest instance (a guard believing a lie)
needs a *speaker* and a *threshold* — both L2. So the smallest genuine slice is
L0 + the thinnest L1: perception populates belief, the planner plans over it, and
ignorance is the one way belief lags truth.

**The reserved seam.** `windows/vessel/src/liveness.rs` already carries the
`Perceived` view precisely for this moment. Its doc comment reads: *"Today its
contents are ground truth; PSY-6's 'plan over belief, not truth' (UNI-16) is
later a change to what fills this, not to the seam."* Today `Perceived {
position, drive }` holds only *self-knowledge* (own location, own thirst — always
true). The Surmise adds the agent's knowledge *of the world* — `believed_water` —
and the ground-truth `water` argument that has been passed around `decide`
dissolves into it. The tick still depends only on `Intent`.

## 3. The design

Belief is a **cache over the truth-ledger** (the governing abstraction). Four
pure pieces, all in `windows/vessel/`:

### 3.1 Truth — where water actually is (L0 substrate)

Water is a region, read from the existing elevation field (no new data): a room
is water iff it lies below an authored level.

- `is_water(room, ctx) -> bool` = `elevation_m(room) <= WATER_LEVEL`. Reuses the
  locale elevation field; the low ground is water, giving **many** sources
  naturally distributed by terrain.
- `nearest_water(from, ctx, budget) -> Option<RoomAddr>`: the true nearest water
  to `from` — a bounded breadth-first walk over the mesh to the closest `is_water`
  room (deterministic, `RoomAddr`-ordered frontier, budget-capped). This is
  *ground-truth-best*; the agent does not know it until it has perceived it.
  `Npc.resource` holds `nearest_water(home)` for reference/tests (the optimal
  target the agent's belief converges toward), not something the agent magically
  knows.
- `downhill_step(from, ctx) -> RoomAddr`: the single steepest-descent neighbour
  (`total_cmp`, `RoomAddr` tie-break — exactly `resource_room`'s existing rule),
  the "water lies low" prior an ignorant agent explores along.

### 3.2 Perception — the coupling channel truth→belief

An agent **perceives water at room R** iff it has stood at R: some committed
`agent-at(agent, R, d ≤ t)` with `is_water(R)`. Perception is the *single* edge
from truth to belief (the connectivity insight — deception, L2, is what adds
other, untrusted edges). Radius 0 (must occupy the room) keeps it minimal and
sufficient; a wider or noisy radius is a followup.

### 3.3 Belief — the cache (L1), nearest-known-of-several

`believed_water(ledger, npc, t) -> Option<RoomAddr>`: a **pure fold over the
agent's existing committed `agent-at` history, gated by water-truth** — among the
water rooms the agent has stood in, the one **nearest to home**, else `None`
(ignorant). No new predicate: belief re-derives from facts already committed
(`agent-at`) ∩ static geography (`is_water`). This is UNI-20's derived view and
the immune essay's *matrix verdict* — the (absent, here) false belief is derived
with provenance, never a second ledger. Nearness-to-home anchors belief to a
stable reference (nearest-to-*current-position* is a followup); as an agent
explores and perceives a nearer source, its believed nearest shrinks — belief
monotonically improves toward the true `nearest_water(home)`.

### 3.4 The planner plans over belief

`Perceived` gains two fields — the agent's perception of the world, distinct from
its self-knowledge:

```
pub struct Perceived {
    pub position: RoomAddr,          // self-knowledge (always true)
    pub drive: f64,                  // self-knowledge (always true)
    pub believed_water: Option<RoomAddr>, // BELIEF (nearest known source; None = ignorant)
    pub downhill: RoomAddr,          // local perceived affordance (which way is down)
}
```

`decide(view, home, p, budget) -> Intent` (the ground-truth `water` argument is
**dropped** — it now lives in the view as `believed_water`):

- thirsty (`view.drive >= act`) **and** `believed_water = Some(w)` → plan to `w`
  and drink (A\*, as today);
- thirsty **and** `believed_water = None` (ignorant) → **explore**: `Do(MoveTo(
  view.downhill))` — a greedy step along the "water lies low" prior, perceiving as
  it goes, until a water room is reached and belief snaps to it;
- not thirsty and away from home → plan home (as today);
- else → `Hold`.

`decide` stays pure and geography-free (the tick fills `downhill` and
`believed_water` from `ctx` + the ledger). The tick builds the view each step from
the frozen ledger (folding `agent-at`-so-far, including this tick's own emitted
moves — so belief updates mid-journey the moment the agent stands in water) and,
as today, commits a dated `agent-at` per move and a `drank` on arrival.
Exploration is step-capped (`MAX_STEPS`, as The Foresight) for termination.

## 4. Architecture & files

`windows/vessel/` only; kernel, domains, and other windows untouched (The Walk
§11). No new kernel primitive — perception uses `RoomAddr::neighbors`, belief and
the A\* planner already exist.

- `windows/vessel/src/liveness.rs` — `is_water`, `nearest_water`, `downhill_step`,
  `believed_water`; `Perceived` gains `believed_water` + `downhill`; `decide`
  reads belief + the explore branch; `DriveMovements::step` builds the view from
  the ledger. (`resource_room` — the old immediate-neighbour helper — is
  superseded by `nearest_water`.)
- `windows/vessel/src/session.rs` — `why`/`recount` prose narrates discovery and
  the believed source ("wandered, having found no water; came upon the mere on day
  N and drank"); no new predicate to register.
- `windows/vessel/tests/` — the keystone (BELIEF ≡ FOLD-OVER-PERCEIVED) and the
  genesis-zero pins.

## 5. The keystone property & acceptance

**BELIEF ≡ FOLD-OVER-PERCEIVED** — the c-series X ≡ reference lineage (INDEX ≡
SCAN, JOIN ≡ SCAN, SCHEDULE ≡ HAND-ORDER, DRIVE ≡ FOLD, A\* PATH DETERMINISM).
Belief is a pure deterministic function of the perceived (`agent-at`) history, and
it *changes behaviour*. The measure-don't-narrate discipline (memory: "4 tests
asserting nothing shipped GREEN"; measuring the value ≠ the mechanism) demands the
tests **force** divergence:

1. **Pure decision divergence (planted).** Two `Perceived` values identical but
   for belief — `believed_water: Some(w)` vs `None` — over a planted geometry
   where the A\*-first-step toward `w` differs from the downhill explore step.
   Assert the two `Intent`s differ, and specifically that the believer's is the
   A\* step toward `w` and the ignorant's is the downhill explore step. (Mutation:
   a `decide` that ignored `believed_water` fails.)
2. **Nearest-of-several divergence (the multi-source keystone).** Two NPCs
   identical in every way — same home, thirst, world — except their pre-seeded
   `agent-at` histories name **different** perceived water sources (NPC A stood at
   the near source W1, NPC B at the far source W2, with a third, nearest source
   the agents have *not* found). Over a thirsty interval, A beelines to **W1** and
   B to **W2** — different destinations, from perceived history alone, and *both
   ignorant of the truly nearest source*. Assert the destinations differ and each
   equals the agent's own believed-nearest. (Mutation: belief ignored ⇒ both go to
   the same true-nearest ⇒ fails.)
3. **Belief improves as it populates (graded ignorance → discovery).** A fresh NPC
   (no perceived water) explores downhill, reaches *some* water, drinks; a later
   trip beelines to it; and if exploration meanwhile perceives a nearer source,
   `believed_water` switches to it. Assert the discovery trip differs from the
   later beeline, and that perceiving a nearer source shrinks the believed
   distance-to-home.
4. **Fold determinism + reload-stability + per-agent isolation** for
   `believed_water` (as `drive_at` has), and **BELIEF ≡ FOLD** re-derivation after
   serialize→reload.
5. **Termination** of the explore loop under any input (the `MAX_STEPS` backstop,
   with a regression test — this arc's recurring bug class).
6. **Genesis byte-identity:** the existing genesis-zero pins (no `agent-at`, no
   `drank`) still build a real seed-42 world and assert zero; no new predicate is
   registered at genesis.

## 6. Determinism & save-format (leads the G3 review)

The flagship determinism result: **belief at zero save-format cost.** No new
committed predicate — belief is a pure derived view over the *existing* `agent-at`
facts ∩ static water-truth (`is_water` over the elevation field). No epoch, no
census regen, genesis byte-identical. The perception/belief/water functions are
pure (reload-stable, no `HashMap`, no RNG, `total_cmp` for all float ordering; the
`nearest_water` BFS frontier is `RoomAddr`-ordered). `decide`'s seam is preserved:
the body-swap changes what *fills* the water input (belief vs truth), moving it
into the view; the tick still depends only on `Intent`.

The one legitimate artifact update is the possess-session over-time **transcript**
(agents now discover water before beelining) — a game-behaviour change, re-baselined
at close, exactly as The Quickening / The Wanting / The Foresight each did. Not a
determinism or census concern.

## 7. Non-goals (reserved — the seam's next body-swaps)

- **Staleness / cache invalidation** (the false-positive polarity). Truth is
  static here, so belief is never stale; giving water truth-volatility (depletion,
  a moving source) is the reserved next polarity (followup 1).
- **Deception / the L2 credulity threshold** (lies, illusion, forgery, charm — a
  second, untrusted truth→belief channel weighted by a signal-detection threshold).
  The next campaign, "The Discernment" (followup 2).
- **Nearest-to-current-position belief & a rich believed-map** — belief anchors to
  home here (a single stable scalar); a per-agent sparse overlay of remembered
  geography, re-anchored to the moving agent, is a followup (followup 4).
- **Probabilistic / noisy perception** — a binary spatial filter here (followup 5).
- **Belief for the possessed player, monsters (L3), population belief (L4)** —
  followups 3, 7.

Domains are untouched; no MAP-27 verb DSL; no arbitration or personality-cost
(those are The Foresight's reserved seams, still reserved).

## 8. Judgment calls (the authored constants)

- **Perception radius = 0** (occupy the room to perceive its water). Minimal;
  wider/noisy radius is a followup.
- **`WATER_LEVEL`** — the elevation at or below which a room is water. Chosen so
  the low ground yields several sources within a short walk of a settlement
  (ignorance bites, but a discovery walk resolves within a `wait`), and so
  seed-42's homes sit above it (an NPC starts on land, ignorant of the nearby
  meres).
- **Exploration = greedy steepest-descent** (the "water lies low" prior), capped
  by `MAX_STEPS`. Deterministic, RNG-free; smarter (info-gain) exploration is a
  followup.
- **`nearest_water` search budget** — bounds the truth-side BFS (Hold if no water
  within it), as `PLAN_BUDGET` bounds the planner.

## 9. Task shape (for the plan)

Roughly five TDD tasks: (T1) `is_water` + `nearest_water` + `downhill_step`
geography helpers over the elevation field; (T2) `believed_water` — the
nearest-of-several belief fold + BELIEF ≡ FOLD keystone / reload / isolation; (T3)
`Perceived` + `decide` belief-and-explore branches (pure decision-divergence test);
(T4) the tick builds the view + the multi-source & discovery keystones + genesis-zero
pins + termination; (T5) `why` discovery prose + book chronicle + close.

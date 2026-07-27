# The Threshold — making the fine layer live

**Campaign:** The Threshold — the derivation and occupancy campaign named in
The Hearth's spec §9.1. The Hearth's **other half**, not its successor.
**Date:** 2026-07-25
**Status:** Shipped 2026-07-26 (campaign *The Threshold*, registry `CLIENT-fine-layer-live`). **The preregistered prediction in §6 was FALSIFIED** — four nulls, each eliminating a candidate explanation; see the chronicle and §6.1's disposition in the ledger.
**Parent:** `2026-07-25-the-hearth-design.md` §9.1 and §11; the Rose Window
metaplan **Amendment 1** (§1a).
**Branch:** `the-threshold`, off `the-hearth` — this campaign consumes The
Hearth's `Interior`, vocabulary, patterns and fields, none of which exist on
`main`.
**Decisions in force:** 0069 (fine position is never serialized), 0072 (derived
geometry is causal), 0073 (epoch granularity is declared), 0009, 0016.
**Ledger:** `.superpowers/sdd/the-threshold-ledger.md` (15 entries, nine
ideonomy passes, four overturns).

---

## 1. What this is

The Hearth builds an interior nobody can reach. Its own §9.1 says so: v1
derives no `Interior` from a real room and gives creatures no anchor position,
so every live site passes `warmth: None` and the headline outcome is
*demonstrated, not observed*.

This campaign makes it **live**. Two things:

1. `interior_of(room, era)` — a real room gets a real interior.
2. Anchor-valued occupancy within the presence bubble — a creature stands
   somewhere, and a field can be read where it stands.

It ships no rendering and no new player verb. Its visible outcome is that the
seed-42 possession galleries change: a cold creature walks to a fire. That is
the whole program's **first player-observable consequence**, arriving here
rather than at Campaign 4.

### 1.1 Two words this program has been using for one thing

The Hearth says its substrate is "not yet reachable from the sim," meaning
*nothing calls it*. The anchor graph says two anchors are reachable, meaning
*a route connects them*. Adopt the split:

- **live** — wired to a call site that actually runs.
- **reachable** — connected by a route through the anchor graph.

§9.1 then reads "the substrate is not yet *live*", which is also just more
accurate. Proposed upstream to The Hearth.

## 2. What this campaign costs, stated plainly

The Hearth is byte-identical by carve. This campaign is not, and the honest
statement of what is surrendered is **not** "byte-identity" in the abstract:

> **`room/furnishing/v1` becomes a live determinism contract.** Retuning the
> pattern inventory stops being a tweak and becomes an epoch.

The Hearth declares that label while every live site reads `warmth: None`, so
today it versions nothing that can move a world. The moment a creature stands
somewhere and reads warmth there, a different furnishing means a different
drive outcome means different committed history.

### 2.1 The tension between decisions 0069 and 0072, resolved

0069 says entering a room, moving within it and leaving "*cannot* alter the
world, byte-identically and by construction." 0072 says derived geometry is
**causal** and the placement algorithm is therefore a determinism contract.
Both are in force and this campaign is where they meet.

They are compatible under one reading, which this spec adopts and which 0072
already states: **the stored position alters nothing; the consequences
commit.** ("When geometry matters, the *fact* is committed.") 0069's third
consequence is therefore scoped to a causally *inert* fine layer. This program
is deliberately building a causally *live* one, so that sentence should be read
as a claim about storage, not about effects.

### 2.2 The arming trap

The Hearth ships a deliberately minimal inventory — five patterns, "small in
content, complete in structure" — explicitly expecting to grow it. This
campaign makes furnishing causal. **After this campaign lands, adding a sixth
pattern is an epoch**: a larger inventory changes selections, changes
interiors, changes warmth, changes committed drive history. Growth is additive
to the *inventory* and emphatically not to the *world*.

Neither campaign can see this alone. It leads §9.

## 3. Derivation

```
interior_of(room: RoomAddr, era: Era) -> Interior
    culture = settlement/peoples lookup at room  -> Option<...>
    built   = culture.is_some()
    cold    = climate at (room, era)
    compose(selection(seed, built, cold))
```

§9.1 names this `interior_of(room, culture, era)`. That signature is not total:
a `RoomAddr` is a position on the geosphere and culture belongs to the people
whose territory contains it — **a natural room has no culture at all.** The
form above reconciles §9.1 with The Hearth's `selection(seed, built, cold)`
without either being wrong: `built` *is* "is there a culture here," and
wilderness is the `None` branch rather than a special case bolted on.

**Cost and lifetime.** Derived per room, bubble-scoped, cached for the bubble's
life, discarded with it. Nothing persists (0069).

## 4. Occupancy

**Bubble-only, never serialized.** Not a predicate, not a committed fact.
This is not a design choice made here — decision 0069 and
`CLIENT-two-tier-position` settle it, and The Hearth names 0069 in its own
header.

Three things follow, and the third is the one worth writing down:

- The `AnchorId`-orphaning problem does not arise. A committed occupancy fact
  would break on a furnishing epoch, because `AnchorId(self.anchors.len())` is
  a **vector offset**, not a name. An ephemeral occupancy cannot orphan.
- **Promotion on touch stays deferred.** The Hearth's ledger #13 deferred it
  for want of a consumer; standing at an anchor is not touching it, so this
  campaign is still not that consumer.
- **`AnchorId` is an offset wearing an identifier's name, and it is safe only
  because occupancy never persists.** That coupling currently spans two
  documents. *Deliverable: state it in the type's own doc comment*, or someone
  later persists an `AnchorId` in good faith.

### 4.1 There is no entry rule

The Hearth §9.1 says "entry defaulting to the threshold anchor." That is a
**placement** framing, and the metaplan's own pipeline rejects placement —
creatures reason in relations, anchors and goals, *never* coordinates.

A creature arriving through a doorway is at the **seam anchor** because that is
where the room-graph edge lands. Everything after is The Hearth's Task 3
router. The seam anchor is the *base case*, not a rule: the answer when there
is nothing to derive from.

Seeded entry — drawing an entry anchor from the room seed — is rejected on
**stream-consumption** grounds rather than taste: it would make entering a room
consume seeded draws, and stream consumption order is a save-format contract
with its own pin-isolation discipline. The stochastic-looking option is the one
that costs determinism.

### 4.2 A seam belongs to the room-graph edge, not to the interior

`Threshold` is one *species* of a genus this program had not named: **the seam
between scales**, the thing that is simultaneously a room-graph edge and an
anchor. A doorway, a ford and an open field edge are all seams; only the first
is a threshold.

```
              NARROW (chokepoint)         BROAD (whole edge)
  BUILT       doorway, gate  <- common    colonnade  <- rare
  NATURAL     ford, col      <- exception open edge  <- THE COMMON CASE
```

Built and natural are **mirror images.** Indoors the default is a chokepoint
and the wall is impassable; outdoors the entire border is passable and the
chokepoint is the exception.

The Hearth gives each room **one** `Threshold` anchor, while rooms sit on the
geodesic mesh with roughly six neighbours. So a built room with two doorways
already has one anchor for two seams, and an arriving creature is placed at
"the threshold" regardless of which door it used. That is wrong **indoors as
well as outdoors** — the wilderness gap was the visible symptom of a modelling
error in the case The Hearth does model.

**The decomposition.** Interiors own **anchors**; the room graph owns
**seams**; a seam declares which anchor it lands at in each interior it joins.

- A **narrow** seam lands at a distinguished anchor — the doorway actually
  used, which is The Hearth's `AnchorKind::Threshold`.
- A **broad** seam lands at the interior's **hub** — the first `Ground`
  anchor, which is how The Hearth's composition defines the hub. That is the
  only topologically available answer: without coordinates there is no
  "nearest anchor to the north edge," and §2.1 of The Hearth forbids reaching
  for one (*outcomes read topology, never metrics*). The forced answer being
  the metric-free one is a good sign rather than a compromise.

  **Find the hub by kind, never by index.** They coincide today only because
  `Ground` leads `INVENTORY` for both filters, and depending on that would be
  identity-by-position — the same bug class this campaign has now found at two
  other scales (`AnchorId` as a vector offset, a seeded draw keyed by index).

**This is additive to The Hearth, not a replacement.** It ships
`AnchorKind::Threshold`; this campaign's seam model *uses* it as the
narrow-case landing site. Wilderness, having no such anchor, takes the broad
case. **The Hearth needs no change** — which is why this model is absorbed here
rather than amended upstream.

**Wilderness therefore needs no threshold pattern at all.** An earlier draft of
this spec recommended adding a `built: false` threshold-like pattern; that is
retired as the wrong fix, because it papers over the modelling error by giving
wilderness a fake doorway. Outdoors a seam is *informational* — a
classification of where a boundary lies — not *physical*, so modelling it as an
anchor-object is a category error.

## 5. Catch-up

**The problem.** Bubble collapse evaporates the interior (derived) and the
occupancy (0069), leaving only coarse `agent-at`. So on re-entry a creature
resumes at the seam anchor — **a creature warming itself at the fire is back at
the door every time the world looks at it, and therefore never gets warm.**
That is `UNI-32` biting for real, and it produces a measurable observer effect:
creatures near the observer are healthier.

**The mechanism** (Nathan's, adopted over the controller's own): on bubble
entry, re-plan every occupant from its pre-entry state, execute only the
non-committing actions, then tick normally.

The rejected alternative was to *derive* occupancy from committed state. It
works, and it is a second implementation of the planner's own judgment
obligated to agree with it forever — the divergence bug The Ordination's
schedule ≡ hand-order keystone exists to prevent. Using the planner itself
gets agreement for free.

### 5.1 The partition is "does it commit", not "is it movement"

`Drink`/`Rest`/`Eat` each commit a fact (`DRANK`/`RESTED`/`EATEN`). Catch-up
runs **exactly the actions whose effects are ephemeral** — principled rather
than arbitrary, and it stays correct as the action set grows, because each new
action declares its side.

**Coarse `MoveTo(RoomAddr)` commits too** (it writes `agent-at`), so catch-up
may not use it or it fabricates history at bubble-entry time. Catch-up is
**within-room movement only**, which does not exist in the `Action` enum.

> **Deliverable:** `Action` gains a fine variant, `MoveWithin(AnchorId)`.

That is how the two scales meet inside the planner. The Hearth asserts
"movement is two-level" but builds `route_within` as a free function beside
GOAP rather than something GOAP can plan over; this campaign makes the
two-level claim real in the type.

### 5.2 Why the subsequence is coherent, and when it stops being

Executing a plan's movement steps while skipping its actions could give an
incoherent state — arriving at a hearth never lit. It does not here: **no
`MoveTo` precondition depends on a `Drink`/`Rest`/`Eat` effect**, because
movement preconditions are purely adjacency.

That is an accident of the current action set, and it ends the moment an action
gates movement — a barred door needing unbarring, which is The Hearth's own
promotion example.

> **Deliverable:** a test asserting *no movement action's precondition may
> depend on a committing action's effect.* The first violation breaks catch-up
> silently, and silently is the bad part.

### 5.3 Catch-up is exact, then approximate

```
  0 turns      one turn       a day           a season        a year+
  |------------|--------------|---------------|---------------|
  seam anchor  exact replay   scaled budget   budget cap      place at
  (base case)                                 reached         drive-optimum
  <-------- EXACT ---------> <-------- APPROXIMATE ---------->
```

Elapsed time scales GOAP's existing `budget: usize`. Beyond a named cap,
catch-up stops replaying and places the creature at its drive-preferred anchor.
Event sourcing bounds unbounded replay with snapshots; nothing here persists,
so a cap is the analogue. **Put the test at the crossover**, not in the middle
of either regime.

### 5.4 What catch-up is, structurally

An **event-sourced projection rebuild**: a coarse durable record plus a fine
reconstructible one, rebuilt on observer attach by replaying the system's own
rules from the last durable point — replaying only rules whose effects are
themselves reconstructible. "A projection rebuild must be side-effect free" is
that field's first law, and §5.1 is that law in this domain.

Two consequences carried over from it:

- **Replay divergence across an epoch.** Catch-up replays *today's* rules over
  a *past* interval, so a world played across a furnishing epoch has its
  unobserved intervals silently reconstructed under the new furnishing —
  remembered positions drift with no committed fact moving. This is 0072's
  "reproducible within an epoch, not across one" surfacing somewhere nobody
  looked. It strengthens §2.2: another reason to keep epochs rare.
- **Order-independence.** Two creatures catching up toward the same hearth must
  give the same result in either order. Free today (anchors have no capacity,
  plans are independent); breaks silently when capacity or `beside(host)`
  arrives. Assert it now.

### 5.5 Present tense only

Catch-up reconstructs a position. It does **not** create history — nothing is
committed — so there is no answer to "how long has she been here," and prose
must not invent one.

> *"She stands by the fire"* — sound.
> *"She has been warming herself since dawn"* — fabrication.

The natural narrating instinct runs straight at this, which is why it is a
spec line rather than a footnote.

## 6. Acceptance

Byte-identity is gone, so the health battery becomes the gate. A single test
will not do it: **a null-control-only gate is passed by The Hearth v1 doing
nothing** — chronicity stays zero and every distress run recovers today, with
`warmth: None` everywhere. A gate the previous campaign already satisfies is
not a gate.

The protocol; **order is load-bearing**, and steps 1–2 are worthless if they
run late.

- **Step 0 — the positive test.** Warmth is readable at every hearth across a
  sample of rooms, and every interior's seam anchor is routable from its
  hearth. Per anchor-kind, never a world average. *Every other step is
  negative; an `interior_of` that silently returned hearthless interiors
  everywhere would pass all of them.*
- **Step 1 — freeze the baseline from The Hearth's tip, before any code.**
  Seed-42 galleries plus the health battery. Late, it aliases other campaigns'
  physics into ours.
- **Step 2 — preregister stratified predictions with signs, before any code.**
  Not "thermal distress falls" but *which subpopulation moves and which must
  not*: cold-climate creatures in built rooms with a hearth improve;
  warm-climate and hearthless-room creatures are unchanged. A world *mean*
  could sit flat while both moved in opposite directions, and a flat mean would
  read as "byte-identity nearly held" — exactly backwards.
- **Step 3 — the battery is a two-sided assertion, not a lookout.** The
  campaign's two halves are asymmetric: `interior_of` alone is **still
  byte-identical** (nobody stands anywhere), and occupancy alone is not
  shippable. So there is **exactly one task boundary at which byte-identity
  dies, and it is known before any code is written.** Identical before it,
  moved after it. Any drift before it is a bug, not a finding.
- **Step 4 — the paired control at close.** Identical seed, identical
  everything, fine layer live vs forced inert. The project's own divergence
  method turned on itself; converts "is this drift acceptable" (a judgment)
  into "is this drift caused by what we built" (a measurement).
- **Step 5 — residual drift outside the predicted strata is named
  creature-by-creature with a physical reason, or the campaign does not
  close.** A backstop behind a real test, not the test.

**Negative check on catch-up:** if creatures near the observer still read
healthier after catch-up, catch-up is not working.

### 6.1 The headline criterion

Not "a cold creature crosses the room to the fire" — that is The Hearth's unit
demonstration one level up. The property this campaign exists to establish is:

> **No drive is permanently unsatisfiable as a function of observation
> pattern.**

Narrower, sharper, and directly checkable.

## 7. Scope

**In:** `interior_of`; anchor occupancy in the bubble; **the seam model** (§4.2 — seams keyed to room-graph edges, narrow and broad);
`MoveWithin(AnchorId)`; catch-up with its cap; the movement-precondition
test; the acceptance protocol.

**Out, each with a home:** rendering and any coordinate solve (Campaign 4);
projective relations and concealment (the vocabulary's third slice); promotion
on touch (the first modification campaign — its `promoted → orphaned`
transition is drawn in the ledger's state machine so that campaign inherits it
rather than rediscovering that epochs orphan promoted anchors); items and
custody; ladder rungs 2–4; lazy per-creature catch-up (unlocked by concealment,
see §5 — concentration at entry is *forced* today because a room is wholly
visible on arrival).

## 8. Findings owed upstream to The Hearth

Produced by this brainstorm, cheap to fix there, expensive to fix after.

1. ~~**BLOCKER — wilderness has no seam anchor.**~~ **RETIRED** by §4.2. It
   was real — `the-threshold` carries `built: true`, so
   `selection(seed, built=false, cold)` returns `[the-water, the-fallen-log]`
   and no `Threshold` — but the fix this section originally proposed (add a
   `built: false` threshold-like pattern) was wrong, and the finding was the
   symptom rather than the disease. Seams belong to room-graph edges; a broad
   seam lands at the hub; wilderness needs no threshold pattern. **The Hearth
   requires no change.** Its
   `wilderness_draws_natural_patterns_and_no_built_ones` test is fine as
   written, since a wilderness interior legitimately has no threshold.
2. **Get the inventory near its intended size before this campaign arms
   furnishing** (§2.2). Growth afterwards is an epoch regardless of keying.
3. **Key the future seeded draw by name, never by position.** v1's `selection`
   is a pure filter with `_seed` unused, documented as "threaded for the future
   variation draw" — so the reshuffling hazard is latent, not present, and
   lands with that change. Same bug class as `AnchorId`-as-offset, one scale
   up (`UNI-37`, "recurs at every scale").
4. **`compose` is a hub, so the hearth is one hop from everything.** The
   cold-creature demonstration is a single-step route that barely exercises
   `route_within`, and graph distance is degenerate (1–2 hops), leaving field
   decay almost nothing to decay over. An argument for some richer composition
   landing in The Hearth rather than after it.
5. **"Live" vs "reachable"** (§1.1).

## 8a. Reserved — seasonal variation

Raised at review: does "the interior is a pure function of the room" survive
outdoor rooms — snow, foliage, a frozen pond? Mostly, on a principle worth
stating, and with one gap.

**An anchor is a place, not a condition.** The gully exists year-round; only
the water is seasonal. A hearth is a hearth whether lit; a streambed is a
streambed whether wet. So a snowmelt stream is not a seasonal *anchor* — it is
a permanent place carrying a seasonal field. Node stability is a consequence of
what an anchor is, not an assumption about weather.

**Change divides by cyclicity, not by rate:**

| change | example | mechanism |
|---|---|---|
| **periodic** — it returns | snow melts, the lake thaws, the ford drops | a **derived read** at `(room, day)`; no state |
| **monotone** — it does not | a log rots, a house burns, a pond dries for good | a **committed fact**, via promotion on touch |

The monotone half is already designed: The Hearth's promotion-on-touch is
exactly this mechanism, and seasonal-to-permanent change is a **second consumer
for it beyond barred doors**. Only the periodic half needs reserving.

**Reserved shape.** Keep `interior_of` a pure function of the room and put time
in the **read**: whether an edge may be traversed *right now* is a passability
read at the current day, consulted by `route_within` rather than baked into
`connect`. Same shape as the rest of this program — topology stable, reads
live — and it consumes `t_mean`/`t_swing`, which the climate domain already
emits, so it needs no producer work.

**The connectivity consequence, which reaches v1.** The Hearth's first
well-formedness rule is that a composition must yield a **connected** anchor
graph, enforced by `permits`. Under a passability read the *base* graph stays
connected while the *traversable* graph may not — a creature stranded in a room
the validator certified connected. The guarantee weakens to "connected in the
base, possibly not today," and one of two rules must be chosen when passability
lands: either seasonal impassability may never disconnect (checked at read), or
stranding is legal. **Until that is chosen, this campaign treats
`route_within`'s `None` as genuinely reachable** — §7's Thermal branch must
behave sensibly when the hearth is unroutable rather than treating it as
impossible.

**And a threshold nobody has named.** Geomorphology distinguishes weather from
landform: a flood is an event over a stable floodplain until it avulses the
channel. A flood is a field; a hundred floods are a new river course. Whichever
campaign builds seasonal passage owes a stated rule for *when a periodic
process promotes to a monotone one* — without it, a permanently-dried pond gets
modelled as a seasonally-dry one and the world has a pond that resurrects.

## 9. Flagged for G3

1. **[epoch / determinism contract — leads this list]** This campaign arms
   `room/furnishing/v1` as a live contract (§2), and inventory growth
   afterwards is an epoch (§2.2). Catch-up is additionally epoch-sensitive in a
   way a fresh sim is not (§5.4). No genesis change, no new serialized state,
   no census exposure — the epoch is entirely in furnishing.
2. **[process] These two campaigns want to be reviewed together at G3.** The
   Threshold is The Hearth's *other half*, not its successor: a campaign whose
   headline outcome is admittedly "demonstrated, not observed" is the first
   half of one cut in two, and the cut runs along a seam. The arming trap (§2.2)
   is precisely what a per-campaign review cannot see — the same structural
   lesson The Excursion and The Selvage each paid for.
3. **[scope grew after G3 was first presented] The seam model (§4.2) was
   absorbed into this campaign** by owner decision, rather than amending The
   Hearth's approved spec or deferring it. It began as a wilderness blocker and
   turned out to be a modelling error affecting built rooms too: one
   `Threshold` anchor per room, where rooms have ~6 neighbours. Absorbing it is
   cheap **only because it is additive** — The Hearth's `Threshold` becomes the
   narrow-case landing site and needs no edit. Worth confirming that reading.
4. **[parameter] The catch-up cap** (§5.3) is a first-pass tunable whose
   *existence* is load-bearing and whose value is implementation tuning.
5. **[assumption] Everything here is specced against The Hearth's spec and
   plan, not against running code.** The Hearth has no code commits. Two
   checks were run against its actual plan text rather than trusted from
   reading, and both paid — one confirmed the wilderness blocker, the other
   **corrected this campaign's own earlier finding** about inventory keying.
   Interfaces may still move when The Hearth is really built.

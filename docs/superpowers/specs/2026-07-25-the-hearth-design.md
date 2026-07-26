# The Hearth — the room's interior as a relational space

**Campaign:** The Hearth — campaign 1 of The Rose Window as amended
**Date:** 2026-07-25
**Status:** draft (G3)
**Parent:** `2026-07-25-the-rose-window-metaplan-design.md`, **Amendment 1**
(§1a) — read that first; this spec builds only on the amended program.
**Decisions in force:** 0069 (fine position is never serialized), 0072 (derived
geometry is causal), 0073 (epoch granularity is declared), 0009 (models author,
dice roll), 0016 (studies preregister their hypotheses).
**Registry:** `CLIENT-relational-fine-layer` (adopted), `CLIENT-furnishing-ladder`
(rung 1), `CLIENT-language-not-catalogue`, `CLIENT-epoch-granularity`.
**Provenance:** seven ideonomy passes over the metaplan's remaining open
questions, 2026-07-25. Three overturned earlier conclusions; the seventh
attacked the model by negation and failed to move it.

## 1. What this is

A room is currently a graph node with prose attached. This campaign gives its
**interior** a structure: a small graph of named places — a hearth, a doorway, a
bench — with relations among them, derived from authored patterns, and read
per-observer.

It ships **no rendering and no new player-facing verb.** What it ships is the
substrate that talking, reading, researching, and learning will each need, and it
proves that substrate by moving one drive that already exists: a cold creature
crosses the room to the fire.

That is the whole visible outcome, and it is deliberate. The metaplan's first
campaign made nothing new playable either — what is playable became legible.
Here, what is *simulated* becomes situated.

## 2. The model

```
  room r, era t
      │  patterns(culture, t)              [authored inventory, derived selection]
      ▼
  anchors A, relations R                   ← DERIVED, CAUSAL, epoch-bound
      │  + committed deltas (§4)
      │  fields F: A -> warmth/light/sound, decaying over R
      ▼
  observer o at anchor a
      │  perceive(o, F, R)
      ▼
  known relations R_o ⊆ R                  ← per-observer (v2; see §9)
      │
      ├─▶ outcomes read A and R_o only ──▶ facts committed when they matter
      │
      └─▶ render: solve coordinates from R ──▶ picture   ← NOT causal, NOT in v1
```

Everything above the render line is topology and folds over committed facts.
Everything below is presentation, and v1 builds none of it.

### 2.1 The rule that makes the rest cheap

> **Outcomes read topology, never metrics.**

Concealment is a pillar lying between observer and target *on the graph*; earshot
is graph distance ≤ k; reachability is A* over anchors. No outcome may depend on
a distance in a plane.

This is what scopes decision 0072 correctly: the **causal** derived geometry is
the pattern-composed anchor graph, not any metric layout. A future coordinate
solve for rendering is therefore free to be retuned forever without an epoch.
The rule was stress-tested by negation against the cases most likely to force a
metric — thrown objects, missile reach, fire spreading — and each resolves
topologically (fire spreads to *adjacent flammable* things). **It should be
enforced, not merely intended:** see §9's flagged item 3.

## 3. Anchors and the room graph

An **anchor** is a named place inside a room: `hearth`, `doorway-north`,
`bench`, `pool`, `fallen-log`. Anchors are the nodes; relations are the edges.

The fine layer is deliberately **the coarse layer one scale down** — rooms:ways
:: anchors:relations, `agent-at`:room :: `at`:anchor — so the implementation
*reuses* rather than parallels. In particular within-room movement is a new
`SearchSpace` over the anchor graph (`kernel/src/astar.rs:13` — the trait is
generic, integer-costed, and its header already anticipates many state spaces),
**not** a new planner and **not** a lattice. A creature does not need a grid to
walk on; it needs somewhere to walk to.

**Movement is two-level.** Plan over rooms, then within the room. The seam is the
doorway: an anchor that is *also* a room-graph edge. This is why the threshold
appears in the first catalogue (§6) — it is the join between the scales, not
decoration.

**Bounded by the bubble, not by history.** Unlike `believed_water` and
`believed_hazard`, which fold all of history and have twice forced performance
work, the anchor read is a function of the *current* derived graph over one room:
O((anchors + occupants)²) with both small. It cannot go quadratic in session
length because it does not outlive the presence bubble. The one piece that *is*
history-folded is item custody (deferred, §9), and it has the same shape as
`believed_water`, so the known remedy already applies if it ever bites.

## 4. Derived base, committed deltas, and promotion on touch

The anchor graph is **derived base + committed deltas** — `World { seed, ledger }`
one scale down.

The base regenerates; but a creature that bars a door or sets something down has
changed the room, and that must survive. A delta cannot simply reference a
derived slot, because decision 0073 expects layout epochs and an epoch may
regenerate the base out from under it, orphaning the delta.

**The resolution: an anchor is derived — no identity, no storage — until
something interacts with it, at which point it is promoted to a committed
entity** with a real `EntityId`, and deltas reference *that*.

```
  state      identity   survives an epoch   cost
  --------   --------   -----------------   ---------------------
  derived    no         regenerates         nothing
  promoted   yes        yes, unchanged      one entity + its facts
```

The barred door survives because barring it made it real; everything untouched
stays free. This bounds the committed set to exactly what was interacted with,
and it is the discipline The First Mark already established: a mark commits,
everything else re-derives.

## 5. The relation vocabulary — borrowed, not invented

Qualitative Spatial Reasoning has solved this. **Region Connection Calculus
(RCC-8)** gives eight relations that are *jointly exhaustive and pairwise
disjoint*, with a published **composition table** — given `A ρ B` and `B σ C`,
what must hold between `A` and `C`. That table is what makes inference over
relations sound, and it exists, verified, rather than needing invention.

Prepositional semantics splits the vocabulary three ways, and one class behaves
differently:

```
  class          examples                     frame        observer-relative
  ------------   -------------------------    ----------   -----------------
  topological    at, in, contains, adjacent   none         no
  proximal       near, beside                 graph dist   no
  projective     between, behind, visible-to  OBSERVER     YES
```

That the relations the *prose* can say are the relations the sim should hold is
not a coincidence to be enjoyed but the criterion to design against — this is a
prose-first world.

**Three requirements on the vocabulary, all checkable:**

1. **JEPD.** The relation set partitions its space: for any pair there is exactly
   one answer. RCC-8 is eight; Allen's interval algebra is thirteen. A partition
   cannot be padded, which is what structurally prevents the catalogue sprawl
   `CLIENT-language-not-catalogue` warns about.
2. **Declared algebra.** Each predicate declares transitivity, symmetry,
   converse, and directionality — `in` is transitive, `beside` is symmetric,
   `behind` is neither. The concept registry already declares predicates with
   their properties (`kernel/src/registry.rs:117`); this follows that pattern
   rather than inventing a second one.
3. **Complements are free.** Privacy and concealment are not features to build:
   RCC-8's *disconnected* is the privacy primitive and not-visible-to is the
   concealment one. They fall out of the calculus.

**The carve, by reversibility.** The predicate algebra is the least-reversible
piece in the whole program — once consumers depend on `in` being transitive,
changing it breaks them — while the object catalogue is purely additive. So the
vocabulary is specified first and ships in dependency order: **topological
(no observer needed) → proximal (needs graph distance) → projective (needs the
observer machinery)**. v1 ships topological only.

## 6. Patterns: author the inventory, derive the selection

The unit of authorship is **not a room template.** Alexander's patterns are
smaller than a room — *The Fire*, *Alcove*, *Window Place* — and a room is a
composition of them. Authoring whole rooms would be the catalogue failure mode
arriving on the first rung.

The architecture to copy is already in this repo: `domains/language/src/phonology.rs`
builds a per-species **phoneme inventory** plus a syllable structure, with an
`Envelope` gating the draw and a `permits` admissibility predicate. Transposed:

- **One authored pattern inventory**, shared across the world (like the phonetic
  space).
- **Per-culture derived selection** from it — which patterns a people uses is a
  function of what that people already is (climate, materials, sociality,
  sedentism), never authored per-culture. This is the same move The Bane made in
  deriving a creature's threat niche from what it already was.
- **A validator** that rejects ill-formed compositions, as the phonotactic
  validator rejects ill-formed words.

This is decision 0009 exactly: models author, dice roll. And it earns a property
worth designing toward — **a player who has seen a few houses of a people can
predict where the hearth will be in an unfamiliar one**, and be wrong in an
interesting way when the culture differs. That is `UNI-1`'s
inference-over-a-hidden-ruleset thesis with a concrete surface.

**The first well-formedness rule:** a composition must yield a **connected**
anchor graph, or part of the room is unreachable. Composition rules carrying the
weight is the test of whether this is a language or a catalogue; if a later
spec's substance is pattern *count*, it has gone wrong.

**Template reuse is a feature.** Real vernacular architecture is templated; a
people's houses look alike, and that is what makes them a people's houses. A
settlement sharing a layout vocabulary is culture made visible — a signal a
per-room solver would destroy.

## 7. Fields: what an anchor emits

Some anchors emit quantities read at a distance. This is `alarm_field`'s shape,
already shipped three times over (the alarm halo, predator pressure, prey
pressure): a field summed over emitters, read where you stand.

```
  anchor     emits            read by
  --------   --------------   ------------------------------------
  hearth     warmth, light    the THERMAL drive (shipped); reading
  threshold  (sound passes)   earshot, privacy
```

**Decay is over graph distance, not euclidean** — required by §2.1, and
precedented: `alarm_field` already decays over a one-hop graph halo.

The hearth is the campaign's namesake because it is simultaneously the
measurable case and the social one: it feeds a drive that already exists, it has
its own pattern in Alexander (*The Fire*), and it is the canonical gathering
place every later social capability will hang off.

**Objects carry hazard as well as affordance.** A hearth affords warmth *and*
burns. `Hazards` and the derived threat niche already exist, and The Bane already
makes a cold-adapted creature dread heat — so the Danger drive can read the
catalogue on day one with no new mechanism.

## 8. Determinism, labels, and byte-identity

**No genesis change.** Anchors live in the vessel's session, never in worldgen.
No new predicate at genesis, no epoch of an existing label, nothing new
serialized: decision 0069 holds because an anchor has no coordinate to serialize,
and a *promoted* anchor is an ordinary entity whose position is `at(anchor)` — a
relation, not a metric.

**Two new labels, versioned from this commit** (decision 0073, split by blast
radius):

```
  label                  governs                     changes   blast radius
  --------------------   -------------------------   -------   ------------------
  room/furnishing/v1     which patterns a room gets  rarely    large (contents)
  room/layout/v1         (reserved; the metric solve) often    none in v1 — the
                                                                solve is not causal
```

`room/child` and `room/face` are structural and are not touched.

**Byte-identity is expected but must be verified, not assumed.** The warmth field
is **additive** — it can only raise comfort — so a creature already thermally
comfortable is unchanged by construction. That is the additive-latent pattern
this project has now used four times, and its strongest form here would be that
no seed-42 creature is thermally stressed indoors, making the field never sought.

If drift appears anyway, it is **scoped drift to be justified, not absorbed**:
name which creatures moved and why, as The Haunt did. Do not regenerate over it.

**Pin invariants, not values** (decision 0073): anything this campaign adds to
the calibration surface is pinned as an ordering, a sign, or "stays zero" — never
as a value that a furnishing epoch will move.

## 9. Scope

**In v1:**

- The anchor graph: nodes, topological relations, a `SearchSpace` for within-room
  movement, the doorway as the two-level seam.
- The topological third of the vocabulary, with declared algebra and JEPD
  asserted by test.
- A **minimal** pattern inventory — small in content, complete in structure:
  a handful of patterns, real composition rules, a real validator with the
  connectivity rule.
- Fields: warmth from the hearth, read by the existing thermal drive.
- **One natural room type as well as one built one** (§9 flagged item 2).

### 9.1 The two things that keep v1 unreachable — deliberately

Writing the plan surfaced that the headline outcome cannot be *observed* in v1,
only *demonstrated*, because two pieces are missing and neither is small:

1. **Nothing derives an `Interior` from a real room.** There is no path from a
   `RoomAddr` to a composed pattern set.
2. **Creatures have no anchor position.** `agent-at` places a creature in a
   *room*; nothing says which anchor it stands at, so `warmth_at` has no "here"
   to read.

Adding them would give creatures a new positional state inside the bubble with
its own entry rules, would move behaviour, and would therefore surrender
byte-identity and make the health battery the gate rather than a check. That is
its own campaign — **the derivation and occupancy campaign**, whose first tasks
are (a) `interior_of(room, culture, era) -> Interior` and (b) an anchor-valued
occupancy for creatures within the bubble, with entry defaulting to the
threshold anchor. Everything it needs is specified here; it inherits the
vocabulary, the graph, the patterns and the fields intact.

Landing the substrate first, byte-identically, is the same shape as The Snapshot
(campaign 1 of this program): nothing new became playable, and what was already
there became legible.

**Explicitly out, and each has a home:**

- Rendering, panes, any coordinate solve — Campaign 4.
- Projective relations, per-observer reads, concealment — the vocabulary's third
  slice, once the observer machinery exists.
- Items, custody, and the `believed_*`-shaped fold over custody events.
- Rungs 2–4 of the ladder: solver-invented arrangements, historical wear,
  creatures arranging their own space.
- Social protocol (`beside(host)`, seating), which needs projective relations.

## 10. Success criteria

- **A cold creature crosses the room to the fire — demonstrated, not yet
  observed.** Given an interior with a hearth, routing from the doorway targets
  the hearth anchor and the warmth read there exceeds the warmth where it began;
  an identical interior with no hearth offers nowhere warmer, so the creature has
  no reason to move. **This is a unit-level demonstration, and deliberately so:**
  v1 builds no interior from a real room and gives creatures no anchor position
  (§9.1), so every live site passes `warmth: None`. The substrate is proved; it
  is not yet reachable from the sim, which is exactly what makes v1
  byte-identical.
- **The anchor graph is connected** for every composition the validator accepts,
  and the validator **rejects** a composition that would disconnect it — a unit
  test on both sides.
- **The vocabulary is JEPD**: for every pair of anchors in a generated room,
  exactly one topological relation holds — asserted by test over a sweep, not by
  inspection.
- **Declared algebra holds**: `in` composes transitively and the composition table
  agrees with RCC-8's published one for the subset implemented.
- **Culture selects, and selection is derived**: two peoples with different
  climates draw measurably different pattern sets from one inventory, and no
  pattern is authored per-culture.
- **Byte-identity**, verified not assumed: `new --seed 42`, the seed-42
  possession galleries, and every committed artifact are unchanged — or the drift
  is scoped, named, and justified.
- **The health null-control holds** (chronicity stays zero; every distress run
  recovers), and the battery is **re-timed** — the longest sim in the suite is the
  probe, never the possession walk.
- **No outcome reads a metric** (§9 flagged item 3).

## 11. Reserved

- The metric coordinate solve and every pane (Campaign 4); `room/layout/vN` is
  declared now so its epochs are cheap later.
- Projective relations → concealment, sightlines, social protocol.
- Items as entities whose position folds over custody events.
- Ladder rungs 2–4, ending at creatures arranging their own space — where
  Alexander argues the quality actually lives.
- Anchors that are themselves animate (a bound spirit, a haunted mirror), the
  far end of the animacy axis `CLIENT-scribe` already holds.

## 12. Findings owed to **The Action Clock** (campaign 2), before it is specced

Two results of these passes belong to that campaign, not this one, and would be
expensive to retrofit:

1. **Actions with duration need *maintenance* conditions, not just entry
   conditions.** "Research the artifact" requires a workbench, custody, light and
   *uninterrupted time*; the last is a condition that must hold throughout and
   breaks when someone enters earshot. An action clock that checks preconditions
   only at the start cannot express "she was interrupted."
2. **Allen's interval algebra is the time analogue of RCC-8** — thirteen
   relations, jointly exhaustive and pairwise disjoint, with its own composition
   table. If The Action Clock needs to reason about overlapping actions, that is
   the settled vocabulary, and adopting it would make the two campaigns
   structurally symmetric.

## 13. Flagged for G3

1. **[scope] v1 ships no rendering and no new verb.** Its entire visible outcome
   is that a cold creature walks to the fire. This is deliberate — it forces every
   structural piece while staying measurable — but it means the campaign is
   invisible to a player. Confirm that is acceptable for campaign 1.
2. **[scope] Wilderness must be in v1, or the fine layer is absent where most
   agents live.** The health battery's wild fauna are outdoors; if anchors exist
   only in built rooms, the campaign cannot be measured against the population the
   project actually simulates. Recommended: one natural room type alongside one
   built one. Confirm, or accept an explicitly stated blind spot.
3. **[determinism] "Outcomes read topology, never metrics" wants enforcement.**
   It is the constraint that keeps the render solve free of epochs, and decision
   0073's risk 4 analogue applies: a rule of this kind decays to policy unless
   something checks it. The `tools/type-audit` default-deny model is the obvious
   shape (a metric quantity may not cross into an outcome path). Should this be a
   decision record and a checked rule in v1, or intent for now?
4. **[design] Promotion on touch** (§4) is new and unexercised. It is the answer
   to epoch-orphaned deltas and it bounds the committed set well, but v1 barely
   uses it — nothing in scope modifies a room. Ship the mechanism now for the
   epoch guarantee, or defer it with the items work and accept that the first
   modification campaign must solve it under pressure?
5. **[risk] This is a large campaign for a first rung.** Vocabulary, patterns,
   validator, graph, movement, fields. The natural split is
   *vocabulary + graph* first, *patterns + fields* second. Scaling it down is the
   owner's call.

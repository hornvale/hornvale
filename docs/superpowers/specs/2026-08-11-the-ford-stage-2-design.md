# The Ford, stage 2 — the room reads the water

*Campaign spec. Status: G3 review. Follows
[stage 1](2026-08-10-the-ford-design.md) and the pre-spec decomposition in
[the question space](2026-08-11-the-ford-stage-2-question-space.md), which
this spec assumes and does not repeat.*

## 1. What changed since stage 1 was specified

Stage 1's §9 said stage 2 would redefine `Locale.fields.water` and mint
`locale/room/v3`. **That is no longer the design**, for two reasons that
arrived after stage 1 froze.

**The Grain ratified decisions 0121–0124 while The Ford was in flight.** 0124
requires a change *presented as a refinement* to preregister a conservation
criterion, whose natural categorical form is *aggregating the fine values over
one coarse cell reproduces that cell's own value*. The Ford's geometry fails
that outright — 39 of 700 seed-42 river cells carry no polyline and only ~50%
read `Channel` at their own centre. Redefining `water` would therefore be a
refinement claim this campaign cannot honour, and would reproduce The Grain's
measured harm: a room-water change that destroyed 29% of the world's fresh
water at walking depth while 3,350 tests stayed green.

**The question space decomposed into five questions, not one.** Only one of
them is a room-scale point property. Redefining a single field to answer all
five was the original error.

## 2. Keystone — store the quantity, not the classification

Stage 1's keystone was that a river is a line, not an area. Stage 2's is its
consequence for the record:

> **A document stores the measured quantity and a legend for reading it; it
> does not store one consumer's classification of that quantity.**

The nautical chart is the exact prior art: soundings, contours and a datum
travel on the chart, and the *mariner* sets a safety contour from their own
draught. Five consumers with five different cuts do not need one enum to be
right for all of them.

**Cyclicity is what makes this load-bearing rather than tasteful.** Water is
static today, but stage 1 §5.4 deliberately refuses to foreclose seasonality.
Under seasonality a stored **class** goes stale the instant discharge varies,
while a stored **distance** never does — the geometry is fixed and the
threshold moves with `Q`. Storing the classification would quietly break the
one extension the earlier spec was written to protect.

`Locale`'s own doc already agrees: it is *"ground truth, re-derivable, never
stored"* — a view, and a view carries what its readers need.

## 3. The sign needs a durable referent

Signed distance is the campaign's load-bearing quantity — a ford **is** a sign
change. But stage 1's sign is relative to the winning polyline's travel
direction, and the line index is **build-order and must never be serialized**
(it renumbers silently if run construction changes).

The fix is hydrology's own convention: **left bank and right bank, facing
downstream.** Downstream direction is derivable from the retained
`TectonicGlobe.downhill` graph, which is stable across builds and already a
committed part of genesis. That gives the sign a durable meaning, removes the
need to serialize anything build-ordered, and is the term a person would use.

## 4. What must not move

Carried forward from stage 1 §4, all still asserted by test:

1. **`Locale.fields.water` keeps its meaning and its mechanism** — the
   dominant-corner partition. It is a nominal field taking a partition, which
   is what 0121 requires, and it is *not* refined by this stage.
2. **`river_proximity`, carrying capacity, and the toponymic `Steeped` gates
   are untouched.**
3. **Cell-scale `WaterKind` is unchanged** for every class.
4. **No new seed draws.** Stage 2 is a pure read over stage 1's network.

Because nothing here is presented as a refinement of a coarser field, **0124's
conservation criterion does not engage.** That is a framing claim, and the
question-space note records the warning it deserves: the framing that
dissolves an expensive obligation is the one this spec's author reached first.
The invariants in this section are what make the claim checkable rather than
convenient — if any of them moves, the framing was wrong.

## 5. Design

### 5.1 Appended keys, no epoch

`locale/room/v2` gains trailing keys and **keeps its schema tag**. This is the
shape main just used for `cave`: appended after the existing fields, so a
document built before they existed is byte-identical up to the new keys.

| key | meaning |
|---|---|
| `channel_distance` | great-circle angular distance to the nearest channel, **signed: positive on the left bank facing downstream**, quantized at emit |
| `channel_bands` | that room's four band edges — channel, bank, floodplain, terrace — in the same angular units |
| `resolution` | per 0123: which fields are decided at canonical-cell resolution and which at channel resolution |

A consumer that wants the ordinal computes it from those two. A consumer that
wants a different cut takes it.

### 5.2 The convenience classifier is a function, not a field

`windows/locale` exposes the ordinal as a function over a room, returning the
band and the signed distance. Convenience for the common case; not a stored
opinion.

### 5.3 Crossing is a traversal query

Fordability is a property of a **path**, never of a place — a ford is a sign
change, which needs two positions. It lands as a function on `windows/locale`
taking two rooms, not as a field on either.

### 5.4 What this stage does not do

- **No drinkability answer.** "Within a walk" is inexpressible: no length scale
  exists anywhere in `kernel/`, `domains/` or `windows/`. Declaring one is its
  own decision.
- **No riparian conditioning.** Threading a band into `draw_variety` moves
  committed prose output and is a drift event; it belongs in stage 3 with its
  own before/after.
- **No scene emission, no client.** Unchanged from stage 1's staging.

## 6. Determinism and save format

- **No new seed draws**; no new stream label.
- **No epoch.** The schema tag `locale/room/v2` is unchanged; keys are appended
  trailing. A test asserts a document is byte-identical up to the new keys.
- Quantize at emit only. `channel_distance` and `channel_bands` are full
  precision in the compute path and quantized at the serialization boundary.
- **The line index is never serialized.** The sign's referent is downstream
  direction, not run order.

## 7. Preregistered hypotheses

Frozen before the code (decision 0016). Each states a floor and a ceiling.

- **H2-1 — the sign is stable across builds.** Two builds of the same seed
  agree on the left/right sign for **100%** of sampled rooms near a channel.
  Anything below 100% means the referent is still build-ordered.
- **H2-2 — appending is byte-clean.** A `locale/room` document is byte-identical
  to its pre-stage-2 form up to the first new key, on **100%** of sampled rooms.
  This is the no-epoch claim, asserted rather than assumed.
- **H2-3 — the ordinal is reproducible from the stored quantity.** Recomputing
  the band from `channel_distance` and `channel_bands` agrees with the
  function's own answer on **100%** of sampled rooms. If a consumer cannot
  reproduce the classification from what the document carries, the document is
  storing the wrong thing.
- **H2-4 — fords exist and are not everywhere.** Over sampled adjacent room
  pairs whose sign differs, the fraction that are crossable under §8's
  criterion lies in **[0.10, 0.70]**. Near 0 the walk is walled; near 1
  crossing carries no meaning.

## 8. H3's late freeze — stated, with its derivation

Stage 1's H3 was **void, not pending**: its "crossable" clause reduced to a
condition on `Q` alone with a free parameter that slid the answer across
`[0, 1]`, and its first version was dimensionally meaningless besides. Stage 1
recorded that the parameter must be stated here, **with a derivation drawn from
something other than the `Q` distribution**, and explicitly as a *late freeze*
made with stage-1 data in hand — not a preregistration.

**The criterion: a channel is crossable where its full width is less than one
room edge at walk depth, and its discharge is below
`carve::WATERFALL_MIN_DRAINAGE`.**

The derivation comes from the **traversal unit, not the water**: a room edge at
walk depth is the granularity at which a person moves, so "narrower than one
step" is what crossing means to the thing doing the crossing. It is expressible
without a length scale, since a room edge at a given depth is a known angular
quantity on the unit sphere. And it is independent of `Q`'s distribution, which
is precisely what stage 1 said the old parameter was not.

This is a **late freeze**, recorded as such. It was chosen knowing stage 1's
measurements, and its interval (H2-4) should be read with that discount.

## 9. Tasks

1. **The bank convention** — derive left/right from the downhill graph; the
   stable-sign test (H2-1).
2. **The appended keys** — `channel_distance`, `channel_bands`, `resolution`;
   the byte-cleanliness test (H2-2) and the reproducibility test (H2-3).
3. **The traversal query** — crossing between two rooms; H2-4 and §8's
   criterion.
4. **Close** — gate, drift, chronicle, retrospective, and the registry row
   (which stays `spec'd`; stages 3–5 remain).

## 10. Risks

1. **The framing in §4 is the whole spec's load-bearing claim.** If a reviewer
   judges that appending a channel-derived field *is* refining room water in
   substance, 0124 engages and this spec owes a conservation criterion it
   cannot meet. That judgement should be made explicitly at G3, not discovered
   at close.
2. **`resolution` is a new published surface** and will reach any document
   consumer; its shape should match 0123's existing precedent rather than
   inventing a second one.
3. **H2-4's interval is a late freeze** and carries less evidential weight than
   a preregistration. The chronicle must not present it as one.

# The Ford stage 2 — the question space, before any design

*A pre-spec design note. It scopes nothing and decides nothing; it exists
because the first attempt to scope stage 2 bundled five questions into one and
asked the fifth of six procedural steps first.*

## Why this note exists

The stage-2 design was drafted as a single question — "does `fields.water`
change meaning, and therefore does `locale/room` take an epoch?" Ideonomy
(cardinality/scope, then negation) separated that into **five distinct
questions with different scopes and different natural carriers**, and put the
epoch question at step 5 of a 6-step procedure whose step 3 had never been
done.

Step 3 is: *which of these are derivable from what stage 1 already ships?*
The table below answers it **against the code**, because the first version of
this table was the author's assertion and was wrong in three of five rows.

## The five questions

| # | question | scope | natural carrier | who asks it |
|---|---|---|---|---|
| 1 | Am I in open water? | a point | signed distance to the network | the walk; the map render |
| 2 | Can I drink today? | a walk | distance, thresholded by reach | the walk; fauna movement |
| 3 | Does this look like water country? | a neighbourhood | distance + channel width | `draw_variety`'s prose; riparian biome |
| 4 | Is this territory watered? | a territory | `river_proximity` | carrying capacity; toponymy |
| 5 | Can I cross **here**? | a **path** | a sign change between two points | the walk; pathfinding |

## Derivability, verified

| # | derivable from stage 1? | evidence |
|---|---|---|
| 1 | **Yes, with no new plumbing** | `windows/locale/src/lib.rs:232` holds `terrain: GeneratedTerrain`, and `transverse_at` is a public accessor on it. |
| 2 | **No — and this was asserted wrongly** | "Within a walk" is not expressible. A grep for any metres-per-unit, planet radius or world radius across `kernel/`, `domains/` and `windows/` returns **nothing**; §5.3 of the stage-1 spec records that `domains/terrain` works in dimensionless solid angle. A reach can be stated in *rooms* or in *radians*, never in minutes or metres, until something declares a length scale — which is its own decision, not a stage-2 detail. |
| 3 | **Yes, but not for free** | `draw_variety` (`windows/locale/src/grammar.rs:275-280`) takes `(room seed, formation, stratum, substrate)`. Adding a band is a signature change, and the function's own doc records that its order and weights are "the ones this draw has always seen, so the descriptor a room renders is unchanged" — so conditioning it **moves committed prose output** and is a drift event, not an addition. |
| 4 | **Not at room scale** | `river_proximity` exists in `domains/terrain` and feeds carrying capacity, but **nothing in `windows/locale` references it** (grep returns empty). The earlier claim that it was "already shipped, already consumed" was true of demography and false of the room. |
| 5 | **Yes, but never as a room field** | A ford is a sign change, which requires two positions. Negating "the *room* reports water" is what surfaced this: crossability is a property of a traversal, and any design that puts it in a room field has made the same category error this campaign exists to fix. |

**Three of five rows changed on contact with the code.** Row 2 flipped outright,
rows 3 and 4 gained costs the first table did not show.

## What this already rules out

- **Any stage-2 scope that promises a drinkability answer** — row 2 has no
  length scale to stand on. It needs a declared reach convention first.
- **Any design that stores fordability on a room** — row 5.
- **"Condition the riparian prose" as a cheap addition** — row 3 is a
  committed-artifact drift event, and it changes the descriptor a player reads.

## The move borrowed from maritime charting

A nautical chart never answers "is there water here" with one field. It ships
**soundings, depth contours and a datum**, and the mariner sets a safety contour
from their own draught; a Notice to Mariners then discloses survey resolution —
which is decision 0123 reached independently in another domain.

The candidate that follows: **ship the quantity, let each consumer set its own
threshold.** Stage 1 already emits `(Transverse, f64)` and publishes
`band_edges`, so the enum is a convenience over the primitive rather than the
primitive. Five questions with five different cuts do not need one enum to be
right for all of them.

This is a candidate, not a decision. It is recorded here so the stage-2 spec
argues with it rather than rediscovering it.

## The procedure stage 2 should follow

1. Enumerate the questions — **done, five.**
2. Name each one's carrier and scope — **done, table above.**
3. Verify derivability against the code — **done, and it changed three rows.**
4. Decide which become stored fields, which become functions, which stay
   consumer-side thresholds.
5. *Then* ask whether any existing field's meaning changes → epoch or not.
6. *Then* ask whether decision 0124 applies → only to whatever is **claimed**
   as a refinement.

Step 6 carries a warning about its own author. Whether 0124 binds is partly a
**framing** choice rather than a fact about the code, and the framing that
dissolves the obligation is the one this project's assistant reached first and
should therefore trust least. If stage 2 claims to refine anything, it owes the
conservation criterion and says so plainly.

## What decisions 0121/0123/0124 require, stated once

- **0121** — a nominal field takes a **partition**, never a threshold on a
  blend. Distance-banding *is* a partition and conserves area by construction,
  so it is compliant; this is not a loophole but the rule's own stated reason
  that nearest-corner assignment was already correct.
- **0123** — a document whose view is finer than a field's model **discloses
  that field's resolution**. Whatever stage 2 ships, `locale/room` gains a
  resolution disclosure.
- **0124** — a change **presented as a refinement** preregisters a conservation
  criterion and lands it as a test. The natural categorical form —
  *aggregating the fine values over one coarse cell reproduces that cell's own
  value* — is one The Ford's geometry **fails**: 39 of 700 seed-42 river cells
  carry no polyline and only ~50% read `Channel` at their own centre. That
  failure is a fact to design around, not one to argue away.

# 0717. A layer's rate is its cache key, and a layer may never read faster-changing data

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot, spec §8.2) ·
**Relates:**
[0289](0289-the-map-is-layers-with-distinct-cache-keys.md) (the map is layers
with distinct cache keys — this record supplies the rule that decides which
key), [0716](0716-a-view-consumes-spectra-and-owns-its-observer.md) (the
spectra a layer composes, whose rates differ),
[The Wash](../../book/src/chronicle/the-wash.md)

In the context of *The Wash* giving the world map layers whose inputs change
at very different rates — cover at geological rate, snow and foliage
seasonally, sunlight diurnally, the cursor instantaneously — we decided that
**a layer's rate is its cache key, and a layer may never read data that
changes faster than its own rate** — accepting one declaration per layer and
one assertion over them.

## The ladder

```
  rate           key changes on            example
  ----------------------------------------------------------
  geological     never                     what the ground is made of
  built          a build or destruction    a road, a paved floor
  seasonal       a season boundary         snow, foliage
  diurnal        the sun or moons moving   the illuminant
  per-turn       a turn                    what the observer perceives
  ornamental     an animation tick         surface motion
  instantaneous  cursor movement           selection
```

## Why the second clause is the whole record

The first clause is bookkeeping. The second is the guard, and it exists
because of a specific failure shape: **a layer that reads faster-changing
data than its key renders correctly on the first frame and is frozen
thereafter.** Correct-then-frozen is close to invisible — the first
observation confirms it, and nothing afterwards contradicts it, because the
stale value is a value that was once right.

This is not hypothetical. It was load-bearing on the day the rule landed, in
both directions:

- **It caught a wrong declaration.** The terrain layer was declared
  *seasonal*, because reflectance is seasonal. But a drawn grid holds an
  **ink**, not a reflectance, so its rate is the fastest thing it reads — the
  diurnal illuminant. The declaration failed the assertion before anyone
  believed it.
- **It named the shape of a real defect.** Adding season and illuminant to a
  tile cache's key made lookups correct and left eviction blind: nothing
  dropped a tile of a superseded light, and the cache grew to 1600 entries
  against a bound of 320 while its own capacity test passed, because that
  test drove everything under a single fixed light.

The generalisation worth carrying: **a new key column is a change to
eviction, not only to lookup.**

## Consequences

- A layer declares its rate and the rates of its inputs; an assertion over
  the declarations fails on any layer reading faster data than its own.
- **A declaration is not an observation, and this record does not pretend
  otherwise.** Nothing derives a layer's declared inputs from what its code
  actually reads. If a layer's reads change and its declaration does not, the
  assertion stays green and says nothing. The mitigation is that the
  declaration sits beside the code it describes; the limit is real and is
  stated here so no later reader mistakes the guard for a proof.
- A layer whose rate has no slot on the ladder is a signal that the ladder is
  incomplete, not that the layer should be forced into a neighbouring rung.

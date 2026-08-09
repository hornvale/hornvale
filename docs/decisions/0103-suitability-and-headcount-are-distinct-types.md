# 0103. Dimensionless suitability and headcount capacity are distinct types

**Status:** Accepted (2026-08-04) · **Decider:** Nathan · **Relates:**
[0008](0008-typed-quantities.md),
[0028](0028-the-bare-ok-rubric.md),
[0092](0092-derivation-at-named-sites.md)

In the context of *The Keeping*'s Task 0 discovering that a campaign spec had
been written on the belief that a dimensionless suitability field was a capacity
field, facing the fact that both are `CellMap<f64>` and therefore
indistinguishable to the compiler, the reviewer and the type-audit alike, we
decided that **a dimensionless suitability and a headcount capacity are distinct
newtypes wherever they meet**, and that **a `CellMap`'s payload is inside the
type-audit's remit, not outside it** — accepting newtype churn across the
demography and worldgen field APIs.

## The failure this exists to prevent

Two fields, both `CellMap<f64>`, one campaign spec written against the wrong one:

```rust
// domains/demography/src/carrying_capacity.rs — a people-DENSITY. Has units.
pub fn carrying_capacity(geo, inputs) -> CellMap<f64>

// windows/worldgen/src/lib.rs — a saturating suitability in [0, 1].
// Dimensionless. Its last statement is
//     let saturated = supply / (1.0 + supply);          // <= 1 always
//     saturated * temperature.eval(..) * moisture.eval(..)
//               * insolation.eval(..)  * elevation.eval(.., 0.0)
pub fn niche_per_species_k(..) -> Vec<(u32, CellMap<f64>)>
```

`The Keeping`'s spec §3.2 proposed replacing the first with the second. That is a
20–100× silent rescale: measured, the cells clearing the bake's daughter-founding
bar fall from a median capacity of ~30–45 to **0–5 of 40,962 for every species**.
Nothing in the toolchain objected.

**The names are transposed, which is how the error survived review.**
`bake_history_from` calls the *capacity* `suitability`:

```rust
let suitability = hornvale_demography::carrying_capacity(..);
let capacity = CellMap::from_fn(geo, |c| *suitability.get(c) * SETTLERS_PER_CAPACITY);
```

while the function named for **K** — carrying capacity — returns the suitability.
So the two words that would have caught the mistake each pointed at the other's
referent.

## Why the existing guards were silent

0008 mandates newtypes for "coherent physical units crossing API boundaries", and
a people-density per cell is such a unit. 0028's rubric requires a verdict tag on
every primitive at a `pub` boundary. Both passed:

```
carrying_capacity    -> bare-ok(count: return)     a headcount density
niche_per_species_k  -> bare-ok(index: return)     describes the u32 species tag
```

> **CORRECTION (2026-08-04, same day, before this record reached main).** This
> section first claimed the audit "never reaches a container's payload." **That
> is wrong, and the tool says so**: implementing §1 immediately produced four
> findings of exactly that shape —
>
> ```
> kernel:311: untagged primitive at values (new)          <- CellMap<f64> param
> kernel:352: untagged primitive at values (new)
> kernel:387: untagged primitive at return (as_cell_map)  <- &CellMap<f64>
> kernel:392: untagged primitive at return (into_cell_map)
> ```
>
> The audit reaches `CellMap<f64>` at a `pub` boundary and demands a verdict.
> The real blind spot is **granularity, not reach**: it wants one verdict per
> *named position*, so a compound return like `Vec<(u32, CellMap<f64>)>` gets a
> single tag for `return`, and `bare-ok(index: return)` — a true statement about
> the tuple's `u32` — satisfied the checker while the `f64` beside it went
> undescribed. **One tag cannot describe two primitives in one position.**
>
> The decision below is unchanged; only the diagnosis is. §2's rule is now
> "a *compound* position needs a verdict per primitive it contains", which is a
> narrower and more tractable tool change than "teach it to see through
> containers" — it already can.

So one field is a ratio and the other a density, and the position that held both
a species tag and a suitability got one verdict describing only the tag.

## The decision

1. **A dimensionless suitability in `[0,1]` and a headcount capacity are distinct
   newtypes**, with validating constructors, wherever either crosses a `pub`
   boundary. The product typechecks (`Headcount × Suitability -> Headcount`); the
   substitution `capacity := suitability` does not compile.
2. **A compound position needs a verdict per primitive it contains.** The audit
   already reaches a `CellMap<f64>` at a `pub` boundary (see the correction
   above); what it cannot yet express is *two* primitives in one named position,
   so `Vec<(u32, CellMap<f64>)>` must carry a verdict for the `u32` **and** for
   the `f64`, not one for `return`. Until the tool supports that, a compound
   position's tag names the payload that carries units, because that is the one a
   substitution can silently rescale.
3. **The transposed names are corrected at the same time** — the variable that
   holds a capacity is not called `suitability`, and a function returning a
   suitability is not called `_k`. Renaming is part of the fix, not a follow-up,
   because the names are what defeated review.
4. **Saturation that discards magnitude is named where it happens.**
   `supply / (1.0 + supply)` converts a magnitude into a ratio; that is the
   single line that made a suitability out of a capacity, and it carries a
   comment saying so.

## Consequences

- **This is a prerequisite, not a cleanup.** It costs no census rebaseline and no
  world-identity change, which makes it the cheapest step in the sequence and the
  one that makes every later step harder to get wrong. Ordering it after the
  behavioural work — as *The Keeping*'s §8 first did — inverts the risk.
- **Deriving a per-species *capacity* is now expressible.** Preserving supply's
  magnitude (`axis_supply × Π conditions`, no saturation) yields a field with
  headcount units, which is what a per-species bake capacity requires. Under the
  old conflation that design could not even be stated.
- **The audit will grow a class of findings it has never reported.** Every
  `CellMap<f64>` at a public boundary becomes taggable, so expect a wave of
  `pending(wave-N)` entries rather than a clean sweep. That is the point: they
  were always untagged, merely invisibly.
- **Not retroactive beyond the boundary.** Fields internal to a function stay
  bare; this binds what crosses a `pub` signature, exactly as 0008 does.

## See also

`The Keeping` spec §§3.2 (retracted), 8;
`windows/worldgen/tests/keeping_probe.rs` (the measurement, and the
self-correction where the probe first inherited the same conflation it exists to
document).

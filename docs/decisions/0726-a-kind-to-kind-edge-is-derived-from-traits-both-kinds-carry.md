# 0726. A kind-to-kind edge is derived from traits both kinds carry

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Tenon · **Relates:**
[0697](0697-what-an-afforded-site-is-worth-is-a-property-of-the-sleeper.md)
(the one-sided grade this extends),
[0586](0586-every-authored-constant-declares-its-axis-of-variation.md)
(the variation ladder),
[0698](0698-a-kind-is-committable-where-an-anchor-identity-is-not.md)
(the committed kind the consumer reads)

## Context

Hornvale had open component tables keyed by a single `KindId`, but no live
relation between two kinds. The first consumer is rest: the value of sleeping
on a ledge depends on both the sleeper's species and the ledge's surface.

An authored `(species, thing)` matrix makes a new surface cost one row per
species. A general typed graph makes one abstraction for one live edge kind and
still needs to decide what direction means. Both conflict with the existing
M+N idiom: a lock states what it requires, a key states what it carries, and
neither names the other.

## Decision

**A kind-to-kind edge is a function of traits carried by both endpoint rows;
no authored pair cell exists.** The Tenon's first edge is factored into:

- `SleepTraits` on the sleeper, including a substrate-response curve; and
- `RestSurface` on the object, including offer and hardness.

The consumer combines them as

```text
1 + (species_grade - 1) × offer × fit(species_substrate, surface_hardness)
```

No crate names both endpoint rosters' keys. The function lives at the consumer,
and each side remains an ordinary `ComponentStore<KindId, _>` row.

## Consequences and costs

- Adding a surface is one object row, not one row per species. Adding a species
  is one species row, not one row per surface.
- The relation is live rather than nominal: in seed 1234, gully-dwarf grades
  rushes above ledge while drow grades ledge above rushes in rooms containing
  both.
- **A pair the latent traits get wrong cannot be overridden.** A sparse override
  table was rejected because it creates two sources of truth for one question.
- A general graph is deferred until a second edge kind has a live consumer.
  Its first design question is direction: `grows-on`, `family_of`, and
  `rests-on` do not have the same symmetry.
- The remaining addition to the one-kind model is per-instance variation
  derived from `Lineage`; this decision does not build it.

## See also

`domains/species/src/lib.rs` (`SleepTraits`, `substrate_response`);
`windows/vessel/src/affordance.rs` (`RestSurface`);
`windows/vessel/src/liveness.rs` (`grade_of`);
[The Tenon chronicle](../../book/src/chronicle/the-tenon.md).

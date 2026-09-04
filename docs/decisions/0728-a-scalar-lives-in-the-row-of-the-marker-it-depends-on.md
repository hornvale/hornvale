# 0728. A scalar lives in the row of the marker it depends on

**Status:** Accepted (2026-09-04) · **Decider:** Nathan (autopilot) ·
**Campaign:** The Tenon · **Relates:**
[0726](0726-a-kind-to-kind-edge-is-derived-from-traits-both-kinds-carry.md)
(the endpoint traits),
[0586](0586-every-authored-constant-declares-its-axis-of-variation.md)
(the scalar's declared axis)

## Context

The obvious representation kept `SupportsRest` in `ObjectTraits.properties`
and put offer and hardness in a second `KindId`-keyed table. This exact split
has already drifted: `RadiatesHeat` has two carriers, hearth and brazier, while
the warmth scalar's dispatcher still names hearth alone.

A one-way check would catch only one half of that failure. Putting scalar
payloads inside the `ObjectProperty` set would instead make two otherwise
identical properties unequal and break the marker's set semantics.

## Decision

**A scalar whose meaning depends on a marker lives in the same component row as
that marker.** `RestSurface` is an optional field of `ObjectTraits`, and totality
is two-way:

```text
SupportsRest is present  iff  rest_surface is Some
```

The surface values are `universal`: they are properties of the thing kind in
every world and for every sleeper. Species variation enters through the other
endpoint; world variation enters through composition.

## Consequences and costs

- There is one source of truth for whether an object offers rest and how its
  surface participates in the grade.
- Adding a marker now requires its payload in the same construction; adding a
  payload requires the marker. Both missing and orphaned rows fail.
- `ObjectTraits` no longer derives `Eq`, because `RestSurface` contains `f64`.
  This is an honest type cost; forcing equality through quantization would put
  quantization in the compute path.
- The rule is deliberately local, not a mandate to merge every marker and every
  scalar. It applies where the scalar has no meaning without that marker.

## See also

`windows/vessel/src/affordance.rs` (`ObjectTraits`, `RestSurface`, and the
two-way invariant);
[The Tenon chronicle](../../book/src/chronicle/the-tenon.md).

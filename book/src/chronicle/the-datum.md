# The Datum

**July 2026 · outcome: merged — elevation is now a typed, datum-carrying
quantity shared through the kernel, the standing bare-number waiver that
covered it is retired, and the policy that decided where such a type lives is
ratified doctrine (decision 0044)**

## What was attempted

Every domain that touches the ground speaks elevation. Terrain derives it
from crust thickness and isostasy and places sea level against it; climate
cools air by a lapse rate above it and classifies cells beneath it as ocean;
deep time raises and lowers the sea across it; the composition root wires the
field from its producer to every consumer; the scene and locale windows
serialize it into their JSON. Since the tectonic globe first shipped, that
one quantity crossed every one of those boundaries as a bare `f64` — metres,
by documented convention, under a standing waiver in the type audit. It was
the single pervasive exception to the project's rule that a coherent physical
quantity crossing an API boundary is a validating newtype, and the waiver was
honest about being a wart: a convention everyone had to remember, enforced by
nothing.

This campaign retired it. A new kernel type, `ReferenceElevation`, replaced
the bare metres at every boundary the datum crosses — five crates re-typed
end to end — under one non-negotiable constraint: the migration had to be
**byte-identical**. Same seed, same worlds, same almanacs, same committed
artifacts, to the digit. A type migration that moved a single serialized
value would not have been a type migration; it would have been a physics
change wearing one's clothes.

## What the quantity actually is

The type's name is doing precise work. `ReferenceElevation` is metres
relative to the **isostatic reference datum**: zero is the surface height of
a crust of reference thickness floating at buoyant equilibrium on the
mantle. That zero is planet-independent — 0 m means the same physical thing
on every generated world, before any water is placed.

It is emphatically *not* height above sea level. Sea level is itself a value
of this type: the world's ocean fraction picks a percentile of the sorted
elevation field, and the elevation at that percentile — a
`ReferenceElevation` like any other — becomes the sea's stand. So "is this
cell ocean?" is an order comparison between two values of one type, and
depth or height-above-the-sea is a *difference*: subtracting two elevations
yields a bare signed metre delta, which is a different kind of quantity — a
length, with a true zero — and deliberately not a `ReferenceElevation`.

That distinction is the type-theoretic heart of the campaign. On the
classical scales of measurement, elevation is an **interval** quantity: its
zero is a convention, differences between values are meaningful, ratios are
not (a summit at twice the metres of another is not "twice as high" in any
physical sense — move the datum and the ratio changes). An interval type
must therefore carry its zero-convention with it — the datum is load-bearing
meaning, not an implementation detail — and it is promoted together with its
ratio-valued delta, which is why subtraction produces plain metres rather
than another elevation. The doctrine ratified alongside this campaign turns
that classification into a checklist: ratio quantities get full arithmetic,
interval quantities carry their datum and get only comparison, subtraction,
and offset; ordinal and nominal scales are not units at all.

## Where it lives, and why there was no choice

The layering rule is constitutional: a domain depends on the kernel and
nothing else — never on another domain. A quantity spoken by more than one
domain therefore has exactly one legal home, and the campaign's policy
question answered itself: **coherent physical quantities that cross domain
boundaries live in the kernel; single-domain quantities stay in their
domain.** Deep time's ice-volume fraction and eustatic sea-level change
remain where they were born; the elevation datum, spoken by four domains and
three windows, moves to `kernel/src/units.rs`. Decision 0044 records the
rule, the scale-of-measurement classifier, and the boundary treatment as
standing doctrine.

The boundary treatment is the part that keeps the type honest: **rich
inside, contract at the edge.** `ReferenceElevation` is a compute-path type
only. It validates at construction (a non-finite elevation is refused with
the physical reason), compares with a deterministic total order, and never
serializes itself — at every dumb boundary it degrades to the same bare
quantized `f64` it always was, via one accessor. There is no `Display`
(the observer owns the presentation frame, not the quantity), and a
validating newtype must never derive a transparent `Deserialize`, which
would smuggle the very values the constructor exists to refuse past it. The
serialized world neither knows nor cares that the compute path grew a type;
that is what byte-identity means.

## The migration shape

The conversion ran **origin-first**: the kernel type landed, then terrain —
where the datum originates — re-typed its elevation field, sea-level
derivation, and provider boundary in one motion. From that moment every
downstream crate failed to compile, deliberately: the compiler became the
enumeration of every site the datum touches, and the consumers (climate,
deep time) and then the composition root and windows were converted by
walking the error list to empty. At no point did a half-typed boundary
exist — nowhere did a bare `f64` elevation meet a typed one with a silent
assumption about which was which.

The arithmetic never changed. Order comparisons ride the derived ordering;
running maxima use the total-order tie-break the sorting rules already
required; lapse rates and depths subtract into bare metres exactly as
before. The proof was the point: regenerate every committed artifact —
worlds, almanacs, maps, study rows — and diff. The diff was empty. The
type audit, which had waived the datum for months, now finds nothing bare to
waive: sixteen elevation tags retired, and the waiver class survives only
where it names a genuinely different quantity (crust *thickness*, in
kilometres, which is not an elevation and waits for its own family).

## What follows

The doctrine's consequence is a queue, not a monolith: the kernel unit
vocabulary grows one anchored family at a time, each a small byte-identical
campaign of exactly this shape. Temperature went next, days later, reusing
the mold on a quantity with the opposite classification puzzle (see
[Temperature](./temperature.md)); angle waits behind it with genuinely new
machinery, since longitude wraps and latitude clamps and neither is a
re-skin of a linear metre. What this campaign settled is the precedent all
of them stand on: where a shared quantity lives, how it is classified, what
it owes at each boundary — and that the whole conversion can be proven, not
argued, to have changed nothing at all.

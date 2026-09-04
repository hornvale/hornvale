# 0687. A derived surface reads continuous causes, never categorical labels

**Status:** Accepted (2026-09-03) · **Campaign:** The Weft · **Decider:**
Nathan · **Relates:** [0669](0669-a-sites-tier-is-placed-or-derived.md)

In the context of building the derived-feature tier's macro-state recipes
(`windows/worldgen/src/fieldpack.rs`, `weft/kinds.rs`), and facing the
question of what a kind's recipe is allowed to read, we decided that **a
derived-feature recipe reads only continuous per-vertex scalars,
materialized as a `VertexMap<f64>` and passed through `LocaleContext::
blend_at`, and never a categorical label such as `Biome`**, accepting that
some macro causes (rock, drainage, temperature, moisture) must be
independently reimplemented as continuous fields rather than read off an
existing enum, and that a `productivity` field must never be materialized
at all.

## Context

`Biome` is an `enum`. A category cannot be bilinearly blended —
`blend_at` structurally cannot smooth a label, which is *why* the
biome-monotony defect this campaign addresses exists in the first place: a
per-vertex categorical answer read across a 1.126 km walk band paints a
uniform label across the whole band. Feeding a derived kind's recipe from
`Biome` (or any other enum) would import the identical defect one layer
down, defeating the reason the derived tier exists.

`MaterialBuffer` is the counter-example that makes derivation possible at
all: five continuous `[0,1]` scalars (`silica`, `grain`, `induration`,
`carbonate`, `metamorphic_grade`). Rock is blendable, and `carbonate` is the
karst driver that physically governs spring/seep and cave placement — a
continuous cause standing in for what a categorical "rock type" label could
never supply at facet resolution.

## Decision

Every macro-state input a `WeftKind` recipe reads is a continuous per-vertex
accessor, materialized once per world into a `VertexMap<f64>`
(`fieldpack.rs`) and blended at facet resolution through `LocaleContext::
blend_at` before a kind's own combine step runs. No kind recipe reads a
`Biome` variant, a `Kingdom`, or any other enum directly.

**A `productivity` field must never be materialized**, and this is the
sharper, easy-to-get-wrong half of the rule. `LocaleContext` computes
productivity **blend-then-combine**: blend temperature, blend moisture,
*then* apply a Liebig minimum. A materialized `productivity` `VertexMap`
would compute **combine-then-blend** instead — combine at each vertex,
*then* blend the combined values — and because the Liebig minimum is
non-linear, `blend(f(a, b)) != f(blend(a), blend(b))`. This is not a
last-bit rounding difference; it is a different quantity wearing the right
name. Thicket/brake, the one kind that needs productivity, reads
`temperature`/`moisture` fields directly and applies the Liebig minimum
itself after blending (`kinds.rs`'s `thicket_macro_state`), the same order
`LocaleContext::productivity_with_weights` uses — an independent
reimplementation, not a call into `windows/locale` (worldgen may not depend
on a window it composes).

## Consequence

Every future kind's recipe is constrained by this record before it is
written: find or materialize a continuous accessor for each macro cause it
needs, and if the cause is itself a non-linear combination of other
continuous fields (as productivity is), combine *after* blending, never
before — reimplementing the combine step locally rather than reading a
precomputed combined field. `fieldpack.rs`'s own module doc states the rule
operationally; this record is its durable, project-wide home.

## See also

`windows/worldgen/src/fieldpack.rs` (module doc: "Only continuous causes,
never categorical labels" / "Never add a `productivity` field");
`windows/worldgen/src/weft/kinds.rs::thicket_macro_state`; spec §5.3 ("Read
the continuous causes, never the categorical label"); [The Weft
chronicle](../../book/src/chronicle/the-weft.md).

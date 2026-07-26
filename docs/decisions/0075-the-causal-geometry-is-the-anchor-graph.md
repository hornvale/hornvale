# 0075. The causal derived geometry is the anchor graph, not the metric layout

**Status:** Accepted (2026-07-25) · **Decider:** Nathan · **Refines:**
[0072](0072-derived-geometry-is-causal.md)

Decision 0072 made derived geometry causal, accepting that the placement
algorithm becomes a determinism contract. In the context of building that
geometry, facing the question of *which* derived structure outcomes actually
read, we decided that **the causal structure is the pattern-composed anchor
graph; a metric layout, if one is ever solved for rendering, is not** —
accepting that no outcome may depend on a distance in a plane.

**Context.** 0072 was ratified before the fine layer had a representation. Once
it did, the two candidates separated: a room's interior is a small graph of
named anchors with topological relations, and any coordinates are *solved from*
those relations for display. Everything 0072 wanted from causal geometry —
emergent cover, blocked sightlines, being overheard because of where one stood —
is a topological fact about the graph. Nothing needs the plane.

**The rule.** *Outcomes read topology, never metrics.* Concealment is a screen
lying between observer and target on the graph; earshot is graph distance ≤ k;
reachability is A* over anchors; a field decays per graph step. Stress-tested by
negation against the cases likeliest to force a distance — thrown objects,
missile reach, fire spreading — and each resolves topologically (fire spreads to
*adjacent flammable* things). No forced counterexample was constructible.

**Consequence.** `room/furnishing/vN` — which patterns a room draws, and so the
shape of its graph — is the determinism contract 0072 describes. A future
`room/layout/vN` governing where a solver *draws* things is **not causal**, so
the visual tuning pass stays free forever and legibility can be retuned without
an epoch. That recovers the freedom decision 0073's risk analysis assumed was
being spent.

In v1 (The Hearth) the rule is *vacuously* enforced: no coordinate exists
anywhere in the fine layer, so no outcome can read one. It becomes checkable
rather than vacuous the moment a coordinate type appears, and that is the
entry obligation of whichever campaign introduces one — on the
`tools/type-audit` default-deny model.

**See also.** The Rose Window metaplan Amendment 1 §1a.5; [The Hearth
chronicle](../../book/src/chronicle/the-hearth.md) and its spec §2.1; decisions
[0072](0072-derived-geometry-is-causal.md),
[0069](0069-fine-position-is-never-serialized.md),
[0073](0073-epoch-granularity-is-declared.md);
`CLIENT-relational-fine-layer` and `CLIENT-room-grid-authority` in the idea
registry.

# The Sounding

The living-community engine — a world's social history derived as a coarse
forward-simulation, baked once at genesis and read cheaply thereafter — rested
on a single unproven claim: that it is *computationally tractable* at the scale
a rich world needs. The Sounding is the instrument that answered it. It is a
feasibility benchmark, not the engine: a representative core loop over
**synthetic** inputs, with the real data structures, the real determinism
constitution, and deliberately trivial placeholder dynamics, swept across
communities, species, history length, and coupling density, reporting where
each cost crosses its budget. You take soundings before you sail; this one
measured how deep the water is before committing the fleet.

## What it measures, and the three cost directions

A run is fully specified by six fields — a seed and five numeric dials
(communities *Z*, species *Y*, epochs *A*, mean graph degree, long-range-edge
fraction) — from which four kinds of synthetic state are derived (a species
table, a community table, a sparse connection graph, a capacity field) and an
epoch loop resolves seeded structural-pressure events (grow, found, raid →
displace, collapse) with graph-coupled displacement. Three cost directions are
instrumented, because that is where the anxiety lives: **genesis bake** (derive
the whole history — generous budget), **read** (sample a biography, expand a
role-handle — the sub-millisecond `look` budget), and **present replay** (derive
one graph neighbourhood forward under an intervention). The headline is not
absolute time but *scaling*: the exponent of each cost against each dial, and
above all whether the inter-community coupling stays sub-quadratic — the one
architectural go/no-go that no later optimization can rescue.

## The benchmark found its target on the first run

The preregistered hypotheses, frozen before the readout, were that genesis-bake
scales linearly in communities and epochs, that read is flat, and — the load
bearer — that the coupling stays sub-quadratic. The first run refuted the last
one, exactly as intended: the naive delivery located "the community occupying
node *N*" by a linear scan, making the coupling quadratic in the community
count, and the top sweep point exhausted memory and ran past an hour. That is
the instrument working: it exposed a quadratic coupling that would have been an
architectural dead end, *before* the engine was built on top of it. The fix is
the standard one — a node-to-community index giving logarithmic lookup — and it
restored linearity. The lesson generalizes past this loop: the danger is always
a *find-the-entity-that-matches* operation done by scanning, and the discipline
is that every cross-reference is a maintained index, never a scan.

## The frontier

With the index in place, the whole sweep completes in seconds, and the measured
frontier (on a single machine; timings are wall-clock and machine-dependent, so
the report is committed as evidence rather than a byte-identity golden — only
the sample biographies are pinned deterministic) reads:

- **genesis bake — linear** in both communities and epochs; a thirty-thousand-
  community, five-hundred-year history bakes in a few seconds and holds in tens
  of megabytes;
- **the coupling — flat**: the exponent against graph degree is within noise of
  zero even when degree is pushed from two to two hundred and fifty-six edges
  per node, and against the long-range-edge fraction pushed to the whole graph.
  The event-based coupling touches one neighbour per event and is therefore
  degree-independent by construction — so graph *creation and traversal* stay
  cheap at density;
- **read — sub-microsecond** per operation, orders of magnitude inside the
  interactive budget;
- **replay — milliseconds** for a neighbourhood, inside its budget.

The world-scale blow-up risk — the scariest of the three profiles the vision
spans — is retired, with numbers.

## What it does not claim

The Sounding is the *macro-history* sounding, and only that. It does not measure
the **fine-cognition** profile (an individual's moment-to-moment perception,
planning, and drives), which is bounded by observation rather than world size
and is the game layer's own territory. It does not measure **emergent quality** —
whether the composed layers *read* as rich rather than as mush — which is
cultivated and observed, never gated by a scaling test. And its flat
degree-exponent tests only the *event* coupling; the *diffuse* coupling (a
fixed-point over every edge — trade, cultural diffusion, disease) is a different
cost shape, absent from this spike, and is the target of the next sounding. A
green Sounding retires one risk decisively; it is necessary, not sufficient, and
the discipline it establishes — *sound before you build* — is the durable
result.

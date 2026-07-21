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
epoch loop resolves seeded structural-pressure events (grow, raid → displace,
collapse) with graph-coupled delivery. Three cost directions are instrumented:
**genesis bake** (derive the whole history), **read** (sample a biography,
expand a role-handle — the sub-millisecond `look` budget), and **present
replay** (derive one neighbourhood forward under an intervention). The headline
is *scaling*: whether the inter-community coupling — locating the community a
raid must deliver to — stays sub-quadratic, the one architectural go/no-go no
later optimization can rescue.

## Measure the workload before you trust the timing

The Sounding's most useful lesson was learned by getting it wrong first. An
early version *timed* the loop but never *counted its workload*, and a review
that instrumented the runs by hand found the coupling had fired **eleven times**
— the "coupling stays sub-quadratic" headline was measuring noise, and the
genuine blow-up (a top point that exhausted memory) was an unrelated
runaway-founding process, not the coupling at all. The fix is a discipline now
built into the harness: a **workload census** — every measured phenomenon is
counted, printed in the report, and **floor-asserted**, so a run that fails to
exercise what it claims to measure *aborts with a specific message* instead of
silently reporting a degenerate result. A benchmark must prove its phenomenon
happened, at volume, before its timings mean anything. Re-tuned so communities
genuinely cycle grow → overshoot → raid → collapse, the base config now fires
**210,772 raids** — a real coupling load to measure.

## The frontier — the coupling, *shown* not asserted

Because a naive delivery and an indexed one produce byte-identical worlds (one
alive community per node makes them agree), the sweep can bake the *same* run
under both and contrast them directly. Measured on a single machine (timings are
wall-clock and machine-dependent, so the report is committed as evidence, not a
byte-identity golden; only the sample biographies are pinned deterministic):

- **the coupling, naive linear scan**: bake scales as **≈ Z²·¹** — quadratic,
  the architectural dead end the benchmark exists to catch;
- **the coupling, node→community index**: bake scales as **≈ Z¹·²** —
  near-linear, and at two thousand communities the scan is already **13× the
  index and diverging**. This is the shipping path;
- **read — sub-microsecond** per operation, orders of magnitude inside the
  interactive budget;
- **density — flat**: bake is within noise of constant against graph degree
  (pushed to 256 edges per node) and long-range-edge fraction (to the whole
  graph), because the event coupling touches one neighbour per raid — so graph
  *creation and traversal* stay cheap at density.

So the world-scale question has a demonstrated answer: the coupling is tractable
**with the right data structure** — a node index the real engine must carry —
and quadratic without it. The benchmark's job was to find which; it did.

## What it does not claim

The Sounding is the *macro-history* sounding, and only that. It does not measure
the **fine-cognition** profile (an individual's moment-to-moment perception,
planning, and drives), which is bounded by observation rather than world size
and is the game layer's own territory. It does not measure **emergent quality** —
whether the composed layers *read* as rich rather than as mush — which is
cultivated and observed, never gated by a scaling test. And the coupling it
exercises is *event-based* (a raid picks one neighbour); the *diffuse* coupling
(a fixed-point over every edge — trade, cultural diffusion, disease) is a
different cost shape, absent from this spike, and is the next sounding's target.
A green Sounding retires one risk decisively; it is necessary, not sufficient,
and the disciplines it established — *sound before you build*, and *census the
workload before you trust the timing* — are the durable result.

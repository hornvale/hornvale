# The Staple D3 — the flow returns downhill

**Status:** Draft for G3 review  
**Campaign:** `campaign/the-staple-d3`  
**Predecessor:** The Staple D2, merged 2026-09-06  
**Decision block:** 0916–0925

## 1. Purpose

D2 separated typed subsistence from population and added deterministic local
exchange. D3 asks whether the existing tribute relation becomes a two-way
institution: a patron's accumulated stores leave the patron and return down
the relation as protection, goods, or legitimacy. The settlement function must
then be a derived consequence of that relation, not a label authored at
founding.

D3 is a dynamics rung. It is not implementation-ready until Task 0 has been
run and the mechanism chosen against its result. No code, epoch, or census is
part of this draft.

## 2. Task 0 falsifier (preregistered)

Use the bake's standing tribute relations at `now`. Let `N` be the count of
`History::tribute` relations, corresponding to the existing
`tribute_relations_at_now` stock. Fail the probe if `N == 0`; an empty relation
population cannot falsify a return-flow claim.

Let `C` count those same relation edges whose derived return produces one of
`Function::Trade`, `Function::Cult`, or `Function::Fort`. `Function::Mine` is
excluded because it already comes from the independent working-daughter path.
The activation bar is:

```text
0 < C < N
```

The two dead poles are load-bearing:

- `C = 0`: no relation returns a D3 function; stores remain a sink.
- `C = N`: every relation returns a D3 function; the result is a uniform
  relabeling, not a differentiated relation gradient.

The probe also reports each edge's continuous return components and bins their
derived magnitude. “Gradient” means multiple occupied magnitude bands with
no single threshold accounting for the result; “cliff” means one threshold
accounts for nearly all classified edges. The gradient/cliff label is a
readout, not permission to call either dead pole green.

The full vacuity review and the ideonomy pass are recorded in
`docs/superpowers/ledgers/2026-09-07-the-staple-d3.md`.

## 3. Verified substrate

`Community` currently carries population, non-edible `stores`, and typed
subsistence. `stores` decay by `STORE_DECAY`; tribute currently adds to the
patron's stores and has no meaningful outflow. `Tribute` carries patron,
assessment, establishment time, and the subordinate's last observed
population. `History::tribute` emits only subordinate, patron, and `since`.

`Bake::open` authors `Function::Agrarian` and `Notability::Common`. `Mine` is
already derived when a working daughter is founded. `Trade`, `Cult`, and
`Fort` already exist in the function enum and in the history structure
composer, so D3 supplies a missing cause rather than inventing a vocabulary.

## 4. Design direction

The primary unit is the relation edge. Each standing edge receives a pure,
deterministic return evaluation from continuous causes available in the bake:
the patron's stores and relation portfolio, the subordinate's need and
current stock, and the relation's existing assessment/health state. The exact
conversion is a later design section; this draft deliberately does not invent
three free constants for protection, goods, and legitimacy.

The derived function is computed from the return vector after the continuous
evaluation, following decision 0687: categorical labels are outputs, never
inputs. A patron aggregate may be emitted as a secondary readout, but it is
not the Task 0 unit because a high-degree hub could make every edge appear
successful and force the `N/N` pole.

The return evaluation must be order-independent. It reads a snapshot of the
opening relation state, computes all edge returns, then applies the resulting
flows simultaneously. No edge may observe a store balance already modified by
an earlier edge in the same pass.

```text
standing Tribute edge
        |
        v
continuous return vector
  protection | goods | legitimacy
        |
        +--> simultaneous stores outflow
        |
        +--> derived edge function: Trade | Cult | Fort | Agrarian
        |
        +--> Task 0 count: C / N and magnitude distribution
```

The exact precedence when multiple return components qualify, and whether one
edge may yield more than one D3 function, are intentionally unresolved until
the probe and the design's component calibration are visible.

## 5. Alternatives rejected at G1

1. **Patron aggregate first.** Rejected as the primary witness: one hub can
   turn many edges into the same apparent outcome, hiding edge variation.
2. **Current stores snapshot.** Rejected: D2's existing tribute inflow makes
   positive stores pass without any D3 outflow.
3. **Catchment-derived function.** Rejected by The Hidage: its present-scale
   counterfactual clears the existing ceiling at every tested seed, a uniform
   rescale rather than a differentiated institution.

## 6. Proof obligations before implementation

- Task 0 has `N > 0` and reports `0 < C < N`, or the campaign closes as a
  measured null/rescale with no mechanism shipped.
- The relation-level witness mutates a continuous return cause and observes a
  changed edge classification; mutating an unrelated categorical label must
  not make the witness pass.
- Simultaneous clearing conserves the relevant stores to float tolerance and
  never makes stores negative.
- A two-edge fixture proves order-independence by reversing relation order.
- The derived function remains absent rather than silently defaulting when no
  return component clears its threshold; `Agrarian` is the null output, not an
  authored D3 label.
- The probe's gradient/cliff classification is not computed from the same
  function label it purports to explain; it reads the continuous return
  quantities directly.

## 7. Dynamics cost

If D3 changes the bake's committed history, budget one epoch, a sanctioned
census re-baseline through the sluice, and conversion of history-adjacent
pins from values into invariants. D2's changed populations and typed stocks
mean this cost compounds with earlier calibration rather than replacing it.
The implementation plan must include the epoch and census work as first-class
tasks, not as post-merge cleanup.

## 8. Deferred scope

Debt, obligations, interest, labor claims, currencies, conversion, price
discovery, negotiation, storage loss, transport, and priority institutions
remain D2-deferred. D4 specialization, D5 cities/notability, rent gradients,
and R3 districts remain downstream. D3 also does not choose the exact
catchment shape or revisit `SETTLERS_PER_CAPACITY`.


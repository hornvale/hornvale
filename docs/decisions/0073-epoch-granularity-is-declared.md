# 0073. Epoch granularity is declared, not discovered

**Status:** Accepted (2026-07-25) · **Decider:** Nathan

In the context of a subsystem we expect to re-derive often — the room-furnishing
and layout solvers, now causal under decision 0072 — facing the fact that every
change to a frozen derivation costs an epoch, we decided that **a churny
derivation declares its seed-derivation labels split by blast radius, versioned
from its first commit** — accepting the up-front cost of finer labels and a
notation for pins that survives re-minting.

**Context.** Epoch granularity is fixed when a label is *declared*, not when it is
bumped: a coarse label makes every regeneration expensive. There are ~20
seed-derivation labels workspace-wide and most carry no version suffix at all;
`room/child` and `room/face` already exist and are structural, so they must never
move. The owner expects to burn through many epochs in this area.

**Consequence — two obligations.**

1. **Split by blast radius.** Declare `room/furnishing/vN` (*what* objects a room
   has — rare, large radius, contents move) separately from `room/layout/vN`
   (*where* the solver puts them — frequent, small radius: future outcomes only,
   since history is already committed). Put the churn in the small-radius layer on
   purpose, so that frequent epochs are the cheap ones by construction.
2. **Pin invariants, not values.** Every epoch moves what preregistered studies
   pin, and decision [0016](0016-studies-preregister-hypotheses.md)'s guard
   forbids editing a study to match its new result — so frequent epochs mean
   frequent owner calls unless pins are epoch-durable. Anything a churny
   derivation touches is pinned as an ordering, a sign, family membership, or
   "stays zero," never as a value. The precedent is in the tree: the health
   null-control abandoned `prevalence < 0.02` for the invariant the metric
   actually means (chronicity stays zero; every distress run recovers), because
   loosening the number would have been the seed-shopping 0016 forbids.

This generalizes beyond the client: it applies to any derivation intended to
churn.

**See also.** The Rose Window metaplan Amendment 1 §1a.5; decisions
[0072](0072-derived-geometry-is-causal.md) and
[0016](0016-studies-preregister-hypotheses.md); the stream-label contract in
the root `CLAUDE.md`; `CLIENT-epoch-granularity` in the idea registry.

# The Staple D2 — decision ledger

Campaign: **The Staple D2** — voluntary local exchange beside tribute.
Branch: `campaign/the-staple-d2`. Decision block: **0886–0895**.
Spec: `docs/superpowers/specs/2026-09-06-the-staple-d2-design.md`.
Status: G3 approved; implementation planning active.

## Entries

#1 [G1] — **What is D2's first mechanism?** · **Decision: two typed,
non-convertible subsistence commodities exchanged bilaterally over local
reachability.** One aggregate stock would model redistribution or aid, not
reciprocal exchange. Ideonomy: 1 pass, no overturn; the pass exposed the
one-stock dead mechanism. Capture: spec §§3–4.

#2 [Q] — **What is the stock family?** · **Decision: population, two typed
subsistence stocks, and existing non-edible `stores`; units are fungible only
within type.** Cross-type conversion is a later campaign. Ideonomy: 1 pass,
no overturn. Capture: spec §3.

#3 [Q] — **How are exchange promises represented?** · **Decision: D2
exercises only immediate delivery; typed obligations remain the extensible
future family.** Failed and partial attempts have explicit outcomes but do
not create debt. Ideonomy: 1 pass, no overturn. Capture: spec §§3–4.

#4 [Q] — **How is scarce stock allocated?** · **Decision: reserve local
projected need, then simultaneous deterministic pro-rata clearing. Funded
acyclic chains may continue; unfunded cycles cannot settle.** Ideonomy: 1
pass, no overturn. Capture: spec §3.

#5 [Q] — **What is the probe?** · **Decision: paired control/treatment over
200 seeds, with separate world and attempt denominators. Dead poles are zero
activation and more than half the treatment worlds breaching each existing
demographic bar.** Outcome rates are descriptive. Ideonomy: 1 pass, no
overturn. Capture: spec §4; decision 0826.

#6 [Q] — **What is committed versus derived?** · **Decision: exchange
outcomes are derived study traces and aggregate census candidates; only
irreducible stock inputs and pins become durable simulation state.** Ideonomy:
1 pass, no overturn. Capture: spec §§3–4.

#7 [G3] — **Which existing bars define instability?** · **Decision: use the
history gate's settlement-count sane band `40..=400`, collapse-share ceiling
`0.05`, and alive-at-now floor `50`; count only treatment-only breaches against
the paired control.** These are existing guardrails, not new population
targets. G3 approval received 2026-09-06. Ideonomy: 1 pass, no overturn.
Capture: spec §4; `windows/worldgen/tests/suite/history_tumult.rs`.

#8 [G4] — **Plan review before execution.** · **Decision: proceed with the
five-stage plan at `docs/superpowers/plans/2026-09-06-the-staple-d2.md`.**
Self-review found and corrected the Task 1 dependency inversion: the probe
fixture freezes the report contract before production exchange code; Task 4
is the integrated treatment. It also replaced the vague one-hop reference
with the verified `traversable_neighbors` helper and named the exact existing
bars. Ideonomy: 0 passes — this is a review gate, not a design choice.
Capture: `IMPLEMENTATION_PLAN.md`; the plan.

## Deferred minors and follow-ups

- Verify the specialization input and shortfall insertion point against the
  live tree before dispatching any brief; every identifier in the brief must
  be grepped first.
- Add the two typed resource names to the idea registry if implementation
  makes them stable concepts rather than probe-local names.
- Promote aggregate exchange metrics to the larger census only after paired
  results show they are durable query needs.
- Re-baseline the census after the D2 epoch and convert history-adjacent pins
  to invariants; do not treat the D2 study trace as a permanent ledger.

## G3 review questions

1. Is D2's two-commodity production split acceptable as the smallest genuine
   exchange mechanism? **Approved at G3.**
2. Does the climate-to-city ownership sentence belong in the metaplan now,
   or remain an open D2 design consequence until the probe is measured?
